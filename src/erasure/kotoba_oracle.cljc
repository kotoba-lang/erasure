(ns erasure.kotoba-oracle
  "Runs the shipped decision core.

  `kotoba/erasure_core.kotoba` holds the decisions;
  `resources/erasure/oracle/erasure-core.kir.edn` is what was compiled from it
  and what ships. This namespace is the seam, and it is deliberately thin: it
  resolves a resource, executes an export, and decides nothing.

  ## Why this exists

  The port landed with `erasure.kotoba-parity-test`, which compiles the
  `.kotoba` and requires it to answer exactly as `erasure.gf` / `erasure.matrix`
  / `erasure.lrc` do. That was the right first step and it is still here. But
  two implementations bound by a test are still two implementations
  (ADR-2608112100): what ran was the `.cljc`, and the `.kotoba` was a checked
  replica. Now the decision half of the library asks this seam, and the `.cljc`
  keeps the halves that are not decisions — building a vector, sorting a read
  list, naming a host exception type, and multiplying bytes.

  ## What deliberately does NOT come through here

  `erasure.gf/add`, `mul`, `mul-def`, `pow`, `inv` and `div` stay in the host,
  and that is a measurement, not an oversight. `gf/mul` is called once per byte
  per coefficient inside `gf/scale-add`, which the design (ADR-2607299200
  section 5) names as the one function a SIMD provider replaces.

  MEASURED 2026-08-12, both sides in the same JVM at the pinned compiler/kir
  pair. The machine was at a load average near 300 while this ran, so read the
  RATIO and not the absolute figures — contention inflates both columns, and it
  is the ratio the design turns on:

      kotoba.kir/execute `gf-mul`    9,351,654 ns    host `gf/mul`   1,355 ns
      kotoba.kir/execute `gf-inv`  140,414,652 ns    host `gf/inv`     549 ns

  — roughly 7,000x and 250,000x. A single 4 KiB shard scaled and added is 4,096
  multiplies; one k=16 encode of a 1 MiB stripe is ~16.7 million. Routing those
  through the interpreter would put mechanism in the wrong place, which is the
  same boundary the core's own header draws. The field is still stated once —
  in the `.kotoba` — and `erasure.kotoba-parity-test` is what holds the host's
  tables to it. What moved here is asked per repair plan and per layout, not
  per byte: `cauchy-entry` at ~128 ms a call is 96 calls for a whole k=16/g=6
  matrix, asked once (see `decide`), against ~16.7 million for the bytes.

  ## The guest ABI

  `kir/execute` takes and returns typed values, not host values. Everything
  this library asks is `:i64` in, `:i64` or `:bool` out, and `:i64` is a JVM
  `long` under `:clj` and a `js/BigInt` under `:cljs`. `i64` / `i64-value` are
  that conversion, kept here so no caller has to know which runtime it is on.

  ## Why nothing here builds a set argument

  The core's other half — `cover-missing`, `locally-repairable?`,
  `local-repair-target` and the drain — takes a `[:set :i64]`, which crosses as
  `[[:set :i64] [items…]]` with at most 32 items (MEASURED 2026-08-12: 32 is
  accepted, 33 is refused). Nothing here builds one, and the reason is not the
  limit. MEASURED the same day at this compiler/kir pair: a `[:set :i64]` does
  not work under ClojureScript AT ALL — `kotoba.kir.value/compare-typed-values`
  routes `:i64` through `cljs.core/compare`, which throws
  `Cannot compare 5 to 4` on a `js/BigInt`, and `typed-set-contains` compares,
  so even a one-element set traps inside the guest. Scalar `:i64` exports are
  unaffected and were checked on the same runtime in the same probe.

  Asking those exports on the JVM only would leave the repair rule in two
  places with one of them unchecked, which is the state ADR-2608112100 exists
  to end — so `erasure.lrc` keeps that one decision, marked, and
  `erasure.set-boundary-test` pins the measurement on both runtimes so it
  cannot go stale.

  ## Asking the same question twice

  `decide` caches by argument. The geometry questions — which group is shard i
  in, what is the Cauchy coefficient of column j in row i — are asked again for
  every plan and every encode over the same layout, and at ~128 ms an
  interpreter call on a loaded box that is the difference between a suite that
  finishes and one that does not. The cache key carries `generation`, which
  `register-kir!` / `deregister-kir!` bump, so a substituted core is never
  answered from a cache filled by the shipped one, and the delegation gate
  below is not quietly answering itself.

  This is only safe to do because every question asked through `decide` has a
  small finite argument domain: a shard index, a group number, a matrix
  coordinate, a set of layout parameters. Nothing asked here is a function of
  an erasure pattern, so the number of distinct questions is bounded by the
  layouts in use. The cache is cleared wholesale if it ever passes
  `cache-limit`, so an unusual caller pays in interpreter calls, not memory.

  ## No fallback around a missing artifact

  A missing or unreadable artifact throws. It does not quietly run something
  else, because a silent fallback is how a decision stops being the one that
  shipped.

  ## ClojureScript hosts must register the KIR

  There is no classpath to read a resource from, so `register-kir!` is the only
  way in and `kir` throws without it. That is a real narrowing of what this
  library used to do on that runtime; see the boundary note on `erasure.lrc`.
  `test/erasure/cljs_runner.cljc` does it with node's `fs`, which is what a
  node host would do."
  (:require [kotoba.kir :as kir]
            ;; Both only exist on the branch that has a classpath to read from.
            #?@(:clj [[clojure.edn :as edn]
                      [clojure.java.io :as io]])))

(def cores
  "Oracle id -> the .kotoba it was compiled from, relative to the repo root."
  {:erasure-core "kotoba/erasure_core.kotoba"})

(defn resource-path [id]
  (str "erasure/oracle/" (name id) ".kir.edn"))

(def ^:private registered
  "Pre-parsed KIR, for runtimes with no classpath, and for the test that has to
  prove the host reads this rather than keeping its own copy."
  (atom {}))

(def ^:private generation
  "Bumped whenever the answer to a question could change — i.e. whenever a core
  is substituted or restored. `decide`'s cache key carries it."
  (atom 0))

(defn register-kir!
  "Install a parsed KIR for `id`, bypassing the resource read."
  [id kir]
  (swap! registered assoc id kir)
  (swap! generation inc)
  kir)

(defn deregister-kir!
  "Drop a registration, so `id` reads the shipped artifact again."
  [id]
  (swap! registered dissoc id)
  (swap! generation inc)
  nil)

(defn- read-artifact [id]
  #?(:clj
     (let [path (resource-path id)]
       (if-let [url (io/resource path)]
         (edn/read-string (slurp url))
         (throw (ex-info "shipped decision core is missing — run `clojure -M:test:gen`"
                         {:oracle id :path path}))))
     :cljs
     (throw (ex-info "no classpath on this runtime — register-kir! first"
                     {:oracle id}))))

(def ^:private cache (atom {}))

(defn kir
  "The shipped KIR for `id`, read once."
  [id]
  ;; A registration wins over the cache: it is an explicit instruction, and a
  ;; caller that registers after something already read the artifact means the
  ;; registration, not the read.
  (or (get @registered id)
      (get @cache id)
      (let [loaded (read-artifact id)]
        (swap! cache assoc id loaded)
        loaded)))

(defn signature
  "The shipped declaration of `export`: `:params`, `:param-types`, `:result`.

  Throws if the export is not there, because a host asking for a signature is
  about to build an argument out of it."
  [id export]
  (let [export (symbol (name export))]
    (or (first (filter #(= export (:name %)) (:functions (kir id))))
        (throw (ex-info "shipped core does not declare that export"
                        {:oracle id :export export})))))

(defn param-types
  "Declared parameter types of `export`, in order."
  [id export]
  (:param-types (signature id export)))

;; ── host values that are not guest values ────────────────────────────

(defn i64
  "Host integer -> guest `:i64`."
  [n]
  #?(:clj (long n) :cljs (js/BigInt n)))

(defn i64-value
  "Guest `:i64` -> host integer."
  [n]
  #?(:clj n :cljs (js/Number n)))

(defn call
  "Execute an export of a shipped core. Args and result are guest ABI values;
  see `i64` and `i64-value` for the conversion this library needs."
  [id export args]
  (kir/execute (kir id) (symbol (name export)) (vec args)))

(def ^:private answers (atom {}))

(def cache-limit
  "Distinct remembered questions before `decide` drops the lot and starts
  again. A ceiling on memory, not a policy — every answer it holds is a pure
  function of its key, so forgetting one costs an interpreter call."
  65536)

(defn decide
  "`call` with HOST integer arguments, remembered for as long as the core does
  not change.

  Args are plain host integers here, not guest `:i64`s, and the conversion
  happens on the way in. That is deliberate and it was measured: an earlier
  version keyed the cache on the guest values, and under `:cljs` — where an
  `:i64` is a `js/BigInt`, which is neither `number?` nor an `IHash` — the
  ClojureScript suite ran for over twenty minutes without finishing, against
  under a minute once the keys became host integers. Whatever ClojureScript
  does with a `js/BigInt` inside a map key, it is not finding it again.

  Only for questions with a small finite argument domain; see the namespace
  docstring."
  [id export args]
  (let [key [@generation id (symbol (name export)) args]]
    (if-let [e (find @answers key)]
      (val e)
      (let [v (call id export (mapv i64 args))]
        (swap! answers (fn [m] (assoc (if (< cache-limit (count m)) {} m) key v)))
        v))))


