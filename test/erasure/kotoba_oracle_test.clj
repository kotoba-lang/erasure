(ns erasure.kotoba-oracle-test
  "What keeps the shipped artifact honest, now that it is what runs.

  `erasure.kotoba-parity-test` compiles `kotoba/erasure_core.kotoba` fresh and
  compares it to `erasure.gf` / `erasure.matrix` / `erasure.lrc`. That was the
  whole check while the host had its own copy of the layout, the matrix and the
  repair decision. It is not the whole check any more, because the host no
  longer computes them — it reads
  `resources/erasure/oracle/erasure-core.kir.edn`, and a fresh compile is not
  that file. Two things have to hold that did not have to before:

    1. the shipped artifact IS the current source, compiled
    2. the host actually reads it, rather than having quietly kept a copy

  The second is the one that is easy to lose and impossible to see: a
  delegation that fell back to a host implementation would pass every parity
  test ever written, because a host copy is exactly what those tests compare
  against. So this asks the only question that separates them — swap in a core
  that answers differently and see whether the host follows."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [erasure.codec :as codec]
            [erasure.gf :as gf]
            [erasure.kotoba-oracle :as oracle]
            [erasure.kotoba-oracle-gen :as gen]
            [erasure.lrc :as lrc]
            [erasure.matrix :as matrix]
            [kotoba.compiler.core :as compiler]))

;; --- drift ----------------------------------------------------------------

(defn- gensym-symbols
  "Symbols whose name ends in `__<digits>` — the shape `gensym` produces.

  `gensym`'s counter is per-JVM, so any of these in a shipped artifact would
  make it differ from a fresh compile for a reason that is not drift, and a
  gate that fails constantly is worse than no gate because it gets ignored.
  MEASURED 2026-08-12 at this compiler pin: this core produces NONE, and two
  `clojure -M:test:gen` runs in two JVMs wrote byte-identical files. So the
  drift gate below compares whole values, which is the strongest form, and
  `the-artifact-is-jvm-independent` is what keeps that entitlement honest.

  The compiler DOES synthesise names — `__kotoba_loop_1`, `__kotoba_loop_2` for
  the two `loop`s in the field arithmetic — but those are numbered per module,
  not per JVM. What would introduce real gensyms is an `and` or an `or` in a
  value position, which lowers to a `let` over an `or-tmp__NNNNN`; the core
  writes nested `if`s instead, for a typing reason it states itself, and that
  is what this measurement currently rests on."
  [kir]
  (into #{} (filter #(and (symbol? %) (re-find #"__\d+$" (name %)))) (tree-seq coll? seq kir)))

(defn- renumber-gensyms
  "Rewrite `foo__11099` to `foo__0`, `foo__1`, … in first-appearance order.

  Not used by the gate — used by its FAILURE MESSAGE, to tell the two reasons
  a comparison can fail apart: a stale artifact, or a core that has grown a
  construct the emitter gensyms for. The second needs this normalisation
  switched on, the first needs `clojure -M:test:gen`, and the two look
  identical without it."
  [kir]
  (let [seen (volatile! {})]
    (walk/postwalk
     (fn [x]
       (if (and (symbol? x) (re-find #"__\d+$" (name x)))
         (let [n (or (get @seen x)
                     (let [n (count @seen)] (vswap! seen assoc x n) n))]
           (symbol (str (str/replace (name x) #"__\d+$" "") "__" n)))
         x))
     kir)))

(defn- first-difference
  "A short account of where two KIR values part company: the name of the first
  function that differs, or the first differing top-level key."
  [fresh shipped]
  (or (first (keep (fn [[a b]] (when (not= a b) (:name a)))
                   (map vector (:functions fresh) (:functions shipped))))
      (first (keep (fn [k] (when (not= (get fresh k) (get shipped k)) k))
                   (distinct (concat (keys fresh) (keys shipped)))))))

(defn- drift-message [id fresh shipped]
  (str "shipped KIR for " id
       (if (= (renumber-gensyms fresh) (renumber-gensyms shipped))
         (str " differs from the source ONLY in gensym numbering — the core has"
              " grown an `and`/`or` in a value position, or something else the"
              " emitter gensyms for. This gate must then compare"
              " `renumber-gensyms` of both, and `the-artifact-is-jvm-independent`"
              " is the one to change first.")
         (str " is stale — run `clojure -M:test:gen`. First divergence: "
              (first-difference fresh shipped)))))

(deftest the-shipped-artifact-is-the-current-source-compiled
  (doseq [[id source] (sort-by key oracle/cores)]
    (testing (str id " <- " source)
      (let [shipped (edn/read-string (slurp (io/resource (oracle/resource-path id))))
            fresh (:kir (compiler/compile-source (slurp (io/file source)) gen/target {}))]
        ;; `(true? (= …))` rather than `(= …)`: the values are the whole module
        ;; and clojure.test prints both sides of a failing `=`. The comparison
        ;; is unchanged; what changes is that the failure is readable.
        (is (true? (= fresh shipped)) (drift-message id fresh shipped))))))

(deftest the-artifact-is-jvm-independent
  ;; What entitles the gate above to compare whole values. If this fails, the
  ;; gate is about to start failing for a reason that is not drift.
  (doseq [id (keys oracle/cores)]
    (is (empty? (gensym-symbols
                 (edn/read-string (slurp (io/resource (oracle/resource-path id))))))
        (str "shipped KIR for " id " carries per-JVM gensyms"))))

(deftest renumbering-still-distinguishes-artifacts
  ;; The normalisation is only allowed to erase the gensym COUNTER. It is what
  ;; the failure message reasons with, and if it erased anything else that
  ;; message would misdirect, so: two genuinely different modules stay
  ;; different through it.
  (let [a (:kir (compiler/compile-source
                 "(ns c (:export [f])) (defn f [x :i64] :i64 (+ x 1))" gen/target {}))
        b (:kir (compiler/compile-source
                 "(ns c (:export [f])) (defn f [x :i64] :i64 (+ x 2))" gen/target {}))]
    (is (not= (renumber-gensyms a) (renumber-gensyms b)))
    (is (= (renumber-gensyms a) (renumber-gensyms a)))))

(deftest every-declared-core-actually-ships
  (doseq [id (keys oracle/cores)]
    (is (some? (io/resource (oracle/resource-path id)))
        (str "no artifact for " id))
    (is (some? (oracle/kir id)))))

(deftest a-missing-artifact-throws-rather-than-deciding-anything
  ;; The seam's one refusal. If it fell back instead, the first thing anyone
  ;; would notice is that a decision quietly stopped being the shipped one.
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"shipped decision core is missing"
                        (oracle/kir :not-a-core)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not declare that export"
                        (oracle/param-types :erasure-core 'no-such-export))))

;; --- the ABI the host builds arguments out of -----------------------------

(deftest the-guest-abi-is-what-the-host-assumes
  (testing "the exports the host calls, and their declared shapes"
    (is (= [:i64 :i64 :i64] (oracle/param-types :erasure-core 'layout-admissible?)))
    (is (= :bool (:result (oracle/signature :erasure-core 'layout-admissible?))))
    (is (= [:i64 :i64 :i64 :i64 :i64]
           (oracle/param-types :erasure-core 'generator-entry)))
    (is (= [:i64 :i64 :i64] (oracle/param-types :erasure-core 'cauchy-entry)))
    (is (= [:i64 :i64] (oracle/param-types :erasure-core 'group-of)))
    (is (= [:i64 :i64 :i64 :i64] (oracle/param-types :erasure-core 'role-of))))
  (testing "and the two the host does NOT call, which is where the port stops"
    ;; Their shapes are pinned anyway: `erasure.set-boundary-test` measures why
    ;; they cannot be asked, and it would be measuring the wrong thing if these
    ;; signatures drifted underneath it.
    (is (= [:i64 :i64 :i64 [:set :i64]]
           (oracle/param-types :erasure-core 'locally-repairable?)))
    (is (= :bool (:result (oracle/signature :erasure-core 'locally-repairable?))))
    (is (= [:i64 :i64 :i64 [:set :i64]]
           (oracle/param-types :erasure-core 'local-repair-target)))))

;; --- delegation -----------------------------------------------------------

(def ^:private wrong-core-source
  "Every export the host calls, with the same signature and a deliberately
  different answer.

  `layout-admissible?` says yes to everything, which is wrong in both
  directions at once: parameters `erasure.lrc/layout` used to accept are now
  accepted for a different reason, and parameters it used to refuse are now
  accepted at all. A host that had kept its three asserts would still throw on
  the second kind."
  (str/join
   "\n"
   ["(ns erasure-core (:export [layout-admissible? group-count shard-count"
    "                           group-of group-start group-end local-parity-of"
    "                           local-repair-reads global-parity-index"
    "                           role-of role-index-of cauchy-entry generator-entry"
    "                           max-tolerated-erasures]))"
    "(defn layout-admissible? [k :i64 r :i64 g :i64] :bool true)"
    "(defn group-count [k :i64 r :i64] :i64 (+ (quot (+ k (- r 1)) r) 1000))"
    "(defn shard-count [k :i64 r :i64 g :i64] :i64 (+ k (+ (quot (+ k (- r 1)) r) (+ g 2000))))"
    "(defn group-of [i :i64 r :i64] :i64 (+ (quot i r) 100))"
    "(defn group-start [q :i64 r :i64] :i64 (+ (* q r) 1))"
    "(defn group-end [k :i64 r :i64 q :i64] :i64 (+ (* q r) 3))"
    "(defn local-parity-of [k :i64 q :i64] :i64 (+ k (+ q 500)))"
    "(defn local-repair-reads [k :i64 r :i64 q :i64] :i64 99)"
    "(defn global-parity-index [k :i64 r :i64 i :i64] :i64 -1)"
    "(defn role-of [k :i64 r :i64 g :i64 i :i64] :i64 2)"
    "(defn role-index-of [k :i64 r :i64 g :i64 i :i64] :i64 42)"
    "(defn cauchy-entry [k :i64 i :i64 j :i64] :i64 (bit-xor (+ k i) j))"
    "(defn generator-entry [k :i64 r :i64 g :i64 i :i64 j :i64] :i64 7)"
    "(defn max-tolerated-erasures [k :i64 r :i64 g :i64] :i64 12345)"]))

(defn- with-core
  "Run `f` against a substituted core, then put the shipped one back."
  [kir f]
  (try
    (oracle/register-kir! :erasure-core kir)
    (f)
    (finally (oracle/deregister-kir! :erasure-core))))

(deftest the-host-reads-the-artifact-rather-than-keeping-a-copy
  (let [wrong (:kir (compiler/compile-source wrong-core-source gen/target {}))
        ;; Built from the SHIPPED core, so the tests below are about the
        ;; functions, not about the map they are handed.
        lay (lrc/layout {:k 16 :r 4 :g 6})]
    (testing "the shipped answers"
      (is (= {:k 16 :r 4 :g 6 :l 4 :n 26} (select-keys lay [:k :r :g :l :n])))
      (is (= 1 (lrc/group-of lay 5)))
      (is (= [4 5 6 7] (lrc/group-members lay 1)))
      (is (= 17 (lrc/local-parity-of lay 1)))
      (is (= 4 (lrc/local-repair-reads-for lay 1)))
      (is (= 0 (lrc/global-parity-index lay 20)))
      (is (nil? (lrc/global-parity-index lay 5)))
      (is (= {:kind :data :group 1} (lrc/role lay 5)))
      (is (= 7 (lrc/max-tolerated-erasures lay)))
      (is (= 1 (gf/mul (bit-xor 18 5) (matrix/cauchy-entry 16 2 5)))
          "C[2][5] is 1/(x_2 + y_5), i.e. the inverse of 18 xor 5")
      (is (= (assoc (vec (repeat 16 0)) 5 1) (lrc/generator-row lay 5)))
      (is (:recoverable? (lrc/recovery-plan lay #{5})))
      (is (= [{:op :local :target 5 :reads [4 6 7 17]}]
             (:steps (lrc/recovery-plan lay #{5})))))
    (with-core wrong
      (fn []
        ;; A host that had kept `(quot i r)`, `(+ k q)`, `(gf/inv (bit-xor …))`
        ;; or the three layout asserts would answer exactly as it did above,
        ;; and nothing else in this repository would say so.
        (testing "the layout"
          (let [l2 (lrc/layout {:k 16 :r 4 :g 6})]
            (is (= 1004 (:l l2)) "group-count followed the substituted core")
            (is (= 2026 (:n l2)) "and so did shard-count")))
        (testing "parameters the shipped core refuses"
          ;; k=0 / r=1 / g=0 fails all three of the conditions
          ;; `layout-admissible?` folds together. The substitute says yes.
          (is (map? (lrc/layout {:k 0 :r 1 :g 0}))
              "admissibility followed the substituted core"))
        (testing "the geometry"
          (is (= 101 (lrc/group-of lay 5)))
          (is (= [5 6] (lrc/group-members lay 1)) "group-start and group-end both followed")
          (is (= 517 (lrc/local-parity-of lay 1)))
          (is (= 99 (lrc/local-repair-reads-for lay 1)))
          (is (nil? (lrc/global-parity-index lay 20)) "-1 still means nil, but it is the core's -1")
          (is (= {:kind :global :index 42} (lrc/role lay 5))
              "role-of AND role-index-of followed")
          (is (= 12345 (lrc/max-tolerated-erasures lay))))
        (testing "the generator matrix"
          (is (= 23 (matrix/cauchy-entry 16 2 5))
              "cauchy-entry followed — the substitute returns the xor, not its inverse")
          (is (= (vec (repeat 16 7)) (lrc/generator-row lay 5)) "generator-entry followed"))
        (testing "and the plan built on all of it"
          ;; `erasure.lrc/locally-repairable` still counts erasures itself —
          ;; see `erasure.set-boundary-test` for why — but WHAT IT COUNTS is
          ;; the cover, and the cover is `group-start`, `group-end` and
          ;; `local-parity-of`. So the substituted geometry reaches
          ;; `recovery-plan`, and through it `codec/decodable?` and
          ;; `codec/decode`: the rule moved, the things phrased in terms of it
          ;; did not have to.
          (is (= [{:op :local :target 5 :reads [6 517]}]
                 (:steps (lrc/recovery-plan lay #{5})))
              "the planner repaired shard 5 out of the substituted core's cover"))))
    (testing "restored"
      (is (= {:k 16 :r 4 :g 6 :l 4 :n 26}
             (select-keys (lrc/layout {:k 16 :r 4 :g 6}) [:k :r :g :l :n])))
      (is (= 1 (lrc/group-of lay 5)))
      (is (= 1 (gf/mul (bit-xor 18 5) (matrix/cauchy-entry 16 2 5))))
      (is (thrown? AssertionError (lrc/layout {:k 0 :r 1 :g 0})))
      (is (= [{:op :local :target 5 :reads [4 6 7 17]}]
             (:steps (lrc/recovery-plan lay #{5})))))))

(deftest a-decode-still-round-trips-through-the-delegated-plan
  ;; The end-to-end statement: the layout, the cover boundaries, the roles and
  ;; every coefficient in this path come from the artifact, and bytes still
  ;; come out the other side.
  (let [lay (lrc/layout {:k 16 :r 4 :g 6})
        data (mapv (fn [s] (mapv #(mod (+ (* s 31) (* % 17)) 256) (range 24))) (range 16))
        encoded (codec/encode lay data)]
    (doseq [pattern [#{5} #{0 1 2 3} #{0 1 2 3 4 5} #{20 21 22 23 24 25}]]
      (is (= encoded (codec/decode lay (reduce #(assoc %1 %2 nil) encoded pattern)))
          (str "round trip through " pattern)))))
