(ns erasure.kotoba-parity-test
  "Equality gate between the `.cljc` reference and its `.kotoba` port
  (`kotoba/erasure_core.kotoba`), per ADR-2607299200 section 5.

  The port is compiled here and executed through the KIR interpreter in this
  same JVM, so nothing crosses a runtime boundary and no typed value has to be
  marshalled: each case is generated as a zero-argument `.kotoba` function
  whose whole body is the call under test, and the interpreter hands back the
  integer directly. This is the harness `css.kotoba-parity-test` established;
  it is reused rather than reinvented.

  `kotoba-lang/compiler` is a TEST-ONLY dependency. The library itself has no
  runtime dependency on it — the `.cljc` is what consumers load, and the port
  exists so the decision layer can move to the verified language without
  anyone having to trust that the two agree."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [erasure.gf :as gf]
            [erasure.lrc :as lrc]
            [kotoba.compiler.core :as compiler]
            [kotoba.kir :as ir]))

(def ^:private port-path "kotoba/erasure_core.kotoba")

(defn- strip-ns-form
  "Everything after the port's own `ns` form.

  The harness supplies its own `ns` so it can export the generated probe
  functions; the compiler rejects an entryless library with no export list.
  Scanning stops the moment parens balance, which is inside the `ns` form and
  therefore before any comment, so comment parens are never counted."
  [src]
  (let [start (str/index-of src "(ns ")]
    (loop [i start depth 0]
      (let [c (.charAt ^String src i)
            depth (cond (= c \() (inc depth)
                        (= c \)) (dec depth)
                        :else depth)]
        (if (and (zero? depth) (> i start))
          (subs src (inc i))
          (recur (inc i) depth))))))

(defn- run-cases
  "Compile the port plus one zero-arg `:i64` function per case, and return
  case-name -> value. `cases` is a map of name -> body source."
  [cases]
  (let [names (sort (keys cases))
        probes (map (fn [n] (str "(defn " n " [] :i64 " (get cases n) ")")) names)
        src (str "(ns erasure-core (:export [" (str/join " " names) "]))\n"
                 (strip-ns-form (slurp port-path)) "\n"
                 (str/join "\n" probes))
        kir (:kir (compiler/compile-source src :wasm32-kotoba-v1 {}))]
    (into {} (map (fn [n] [n (ir/execute kir (symbol n) [])])) names)))

(defn- set-literal [xs]
  (str "(typed-set [:set :i64] " (str/join " " (sort xs)) ")"))

;; --- the corpus -----------------------------------------------------------
;; Production parameters from ADR-2607299200: k=16 data, locality 4, six
;; global parities, so l=4 and n=26.

(def ^:private K 16)
(def ^:private R 4)
(def ^:private G 6)
(def ^:private lay (lrc/layout {:k K :r R :g G}))

(def ^:private gf-sample
  "Edge cases plus a deterministic spread. Exhaustive would be 65,536 probe
  functions; `erasure.gf-test` already checks all of them against `mul-def`
  on the .cljc side, so what this gate has to establish is that the port
  computes the same function, not that the field is a field."
  (concat (for [a [0 1 2 3 255 128 127] b [0 1 2 3 255 128 127]] [a b])
          (for [i (range 40)]
            [(mod (* 37 (inc i)) 256) (mod (* 91 (+ 7 i)) 256)])))

(deftest gf-mul-matches
  (let [cases (into {} (map-indexed (fn [i [a b]]
                                      [(str "mul_" i) (str "(gf-mul " a " " b ")")])
                                    gf-sample))
        actual (run-cases cases)]
    (doseq [[i [a b]] (map-indexed vector gf-sample)]
      (is (= (gf/mul-def a b) (get actual (str "mul_" i)))
          (str "gf-mul " a " " b)))))

(deftest gf-inv-matches
  (testing "every element of the field, exhaustively"
    ;; Was a 19-element sample. Exhausting it costs one compile and 256
    ;; interpreter calls, and it is what makes the composition argument under
    ;; `gf-div-matches` sound: `div` is `mul` after `inv`, so a `mul` gate plus
    ;; a total `inv` gate covers pairs the div cases themselves never name.
    (let [xs (range 256)
          cases (into {} (map (fn [a] [(str "inv_" a) (str "(gf-inv " a ")")])) xs)
          actual (run-cases cases)]
      (doseq [a xs]
        (is (= (gf/inv a) (get actual (str "inv_" a))) (str "gf-inv " a))))))

;; --- division -------------------------------------------------------------
;; 65,536 pairs would be 64 compiles (`max-functions` is 1024 and each case is
;; one probe function) and about sixteen minutes of interpreter calls, so this
;; gate is exhaustive in each argument separately rather than in the pair:
;; every divisor against two dividends, every dividend against two divisors,
;; then the axioms. Together with `gf-mul-matches` and the now-total
;; `gf-inv-matches` above, and the fact that `gf-div` is literally
;; `(gf-mul a (gf-inv b))` on both sides, that is coverage of the pair space by
;; composition rather than by enumeration.

(deftest gf-div-exhaustive-in-divisor
  (testing "every divisor, including zero"
    (let [pairs (for [a [1 255] b (range 256)] [a b])
          cases (into {} (map (fn [[a b]] [(str "div_" a "_" b)
                                           (str "(gf-div " a " " b ")")])) pairs)
          actual (run-cases cases)]
      (doseq [[a b] pairs]
        (is (= (gf/div a b) (get actual (str "div_" a "_" b)))
            (str "gf-div " a " " b))))))

(deftest gf-div-exhaustive-in-dividend
  (testing "every dividend"
    (let [pairs (for [a (range 256) b [3 200]] [a b])
          cases (into {} (map (fn [[a b]] [(str "div_" a "_" b)
                                           (str "(gf-div " a " " b ")")])) pairs)
          actual (run-cases cases)]
      (doseq [[a b] pairs]
        (is (= (gf/div a b) (get actual (str "div_" a "_" b)))
            (str "gf-div " a " " b))))))

(deftest gf-div-obeys-the-field-axioms
  (testing "a/a = 1, a/1 = a, and (a/b)*b = a, on the port"
    (let [nonzero (range 1 256)
          spread (for [i (range 120)]
                   [(inc (mod (* 53 (inc i)) 255)) (inc (mod (* 149 (+ 3 i)) 255))])
          cases (merge
                 (into {} (map (fn [a] [(str "self_" a) (str "(gf-div " a " " a ")")])) nonzero)
                 (into {} (map (fn [a] [(str "byone_" a) (str "(gf-div " a " 1)")])) (range 256))
                 (into {} (map (fn [[a b]] [(str "rt_" a "_" b)
                                            (str "(gf-mul (gf-div " a " " b ") " b ")")]))
                       spread))
          actual (run-cases cases)]
      (doseq [a nonzero]
        (is (= 1 (get actual (str "self_" a))) (str "a/a = 1 for a=" a)))
      (doseq [a (range 256)]
        (is (= a (get actual (str "byone_" a))) (str "a/1 = a for a=" a)))
      (doseq [[a b] spread]
        (is (= a (get actual (str "rt_" a "_" b)))
            (str "(a/b)*b = a for a=" a " b=" b)))))
  (testing "the axioms do not catch a wrong polynomial — only parity does"
    ;; Recorded because it is the reason the two deftests above exist at all.
    ;; 0x11B is also irreducible, so GF(2^8) built on it is still a field and
    ;; still satisfies every axiom asserted here; what it is not is the field
    ;; `erasure.gf` implements, or the one every other Reed-Solomon
    ;; implementation interoperates over. An axiom suite alone would pass on a
    ;; port that silently produced incompatible shards.
    (is (= 0x11d gf/field-poly))))

(deftest gf-div-mirrors-the-cljc-zero-divisor-behaviour
  (testing "division by zero is 0 on both sides, not an error on either"
    ;; `erasure.gf/div`'s docstring says \"`b` must be non-zero\" and nothing
    ;; enforces it: `inv` returns 0 for 0, so `(div a 0)` is 0. The port
    ;; mirrors that rather than correcting it, because a parity gate cannot
    ;; both mirror and correct. Pinned here so that if anyone does fix the
    ;; .cljc, this test is what tells them the port has to move with it.
    (let [as [0 1 2 17 128 255]
          cases (into {} (map (fn [a] [(str "dz_" a) (str "(gf-div " a " 0)")])) as)
          actual (run-cases cases)]
      (doseq [a as]
        (is (= 0 (gf/div a 0)) (str ".cljc div " a " 0"))
        (is (= 0 (get actual (str "dz_" a))) (str "port gf-div " a " 0"))))))

(deftest generator-matrix-matches
  (testing "every entry of the generator matrix, exhaustively"
    (let [coords (for [i (range (:n lay)) j (range K)] [i j])
          cases (into {} (map (fn [[i j]]
                                [(str "gen_" i "_" j)
                                 (str "(generator-entry " K " " R " " G " " i " " j ")")]))
                      coords)
          actual (run-cases cases)]
      (doseq [[i j] coords]
        (is (= (nth (lrc/generator-row lay i) j)
               (get actual (str "gen_" i "_" j)))
            (str "generator-entry i=" i " j=" j))))))

(deftest role-and-layout-match
  (let [idxs (range -1 (inc (:n lay)))
        role->tag {:data 0 :local 1 :global 2 :out-of-range 3}
        cases (merge
               (into {} (map (fn [i] [(str "role_" (+ i 1))
                                      (str "(role-of " K " " R " " G " " i ")")])) idxs)
               (into {} (map (fn [q] [(str "gsize_" q)
                                      (str "(group-size " K " " R " " q ")")]))
                     (range (:l lay)))
               {"gcount" (str "(group-count " K " " R ")")
                "scount" (str "(shard-count " K " " R " " G ")")})
        actual (run-cases cases)]
    (doseq [i idxs]
      (is (= (role->tag (:kind (lrc/role lay i)))
             (get actual (str "role_" (+ i 1))))
          (str "role-of " i)))
    (doseq [q (range (:l lay))]
      (is (= (count (lrc/group-members lay q)) (get actual (str "gsize_" q)))
          (str "group-size " q)))
    (is (= (:l lay) (get actual "gcount")))
    (is (= (:n lay) (get actual "scount")))))

(def ^:private erasure-corpus
  "Patterns chosen to exercise each branch of the local-repair decision: a
  clean single loss, a loss plus its own group's local parity, a whole group,
  a global-parity loss, and the empty set."
  [#{} #{5} #{5 17} #{0 1 2 3} #{4 5 6 7 17} #{20} #{0 4 8 12} #{9 10}])

(deftest local-repair-decision-matches
  (let [cases (into {}
                    (mapcat (fn [[i erased]]
                              (cons [(str "first_" i)
                                     (str "(first-repairable-group " K " " R " "
                                          (set-literal erased) ")")]
                                    (map (fn [q]
                                           [(str "cover_" i "_" q)
                                            (str "(cover-missing " K " " R " " q " "
                                                 (set-literal erased) ")")])
                                         (range (:l lay))))))
                    (map-indexed vector erasure-corpus))
        actual (run-cases cases)]
    (doseq [[i erased] (map-indexed vector erasure-corpus)]
      (testing (str "pattern " erased)
        ;; cover-missing, per group
        (doseq [q (range (:l lay))]
          (let [cover (conj (set (lrc/group-members lay q)) (lrc/local-parity-of lay q))]
            (is (= (count (filter erased cover)) (get actual (str "cover_" i "_" q)))
                (str "cover-missing group " q))))
        ;; first-repairable-group agrees with the .cljc planner's first step
        (let [expected (or (first (filter
                                   (fn [q]
                                     (let [cover (conj (set (lrc/group-members lay q))
                                                       (lrc/local-parity-of lay q))]
                                       (= 1 (count (filter erased cover)))))
                                   (range (:l lay))))
                           -1)]
          (is (= expected (get actual (str "first_" i)))
              "first-repairable-group"))))))

(deftest single-loss-costs-exactly-r-reads
  (testing "the claim ADR-2607299200 makes about repair traffic, on both sides"
    (let [cases (into {} (map (fn [q] [(str "reads_" q)
                                       (str "(local-repair-reads " K " " R " " q ")")]))
                      (range (:l lay)))
          actual (run-cases cases)]
      (doseq [q (range (:l lay))]
        (is (= R (get actual (str "reads_" q)))
            "a full group's single-shard repair reads exactly r shards")
        (let [plan (lrc/recovery-plan lay #{(* q R)})]
          (is (= 1 (count (:steps plan))))
          (is (= :local (:op (first (:steps plan)))))
          (is (= R (count (:reads plan)))
              "the .cljc planner agrees"))))))

(deftest role-index-matches
  (testing "the :group / :index half of erasure.lrc/role, which role-of does not carry"
    (let [idxs (range -1 (+ 3 (:n lay)))
          cases (into {} (map (fn [i] [(str "ridx_" (+ i 1))
                                       (str "(role-index-of " K " " R " " G " " i ")")]))
                      idxs)
          actual (run-cases cases)]
      (doseq [i idxs]
        (let [{:keys [kind group index]} (lrc/role lay i)
              expected (cond (= kind :out-of-range) -1
                             (= kind :global) index
                             :else group)]
          (is (= expected (get actual (str "ridx_" (+ i 1))))
              (str "role-index-of " i " (kind " kind ")")))))))

(deftest global-parity-index-matches
  (testing "nil becomes -1, including the .cljc's unbounded top edge"
    ;; `erasure.lrc/global-parity-index` checks only `(>= i (+ k l))`, so past
    ;; the last shard it answers with a Cauchy row number for a row that does
    ;; not exist. The range here deliberately runs past `n` so that behaviour
    ;; is pinned rather than accidentally never exercised.
    (let [idxs (range -1 (+ 4 (:n lay)))
          cases (into {} (map (fn [i] [(str "gpi_" (+ i 1))
                                       (str "(global-parity-index " K " " R " " i ")")]))
                      idxs)
          actual (run-cases cases)]
      (doseq [i idxs]
        (is (= (or (lrc/global-parity-index lay i) -1)
               (get actual (str "gpi_" (+ i 1))))
            (str "global-parity-index " i))))))

(deftest layout-admissible-matches
  (testing "the parameters erasure.lrc/layout refuses, as a decision instead of an AssertionError"
    (let [configs [[16 4 6] [8 4 3] [1 2 1] [2 2 254]     ; admissible, incl. the k+g edge
                   [0 4 6] [16 4 0] [-1 4 6] [16 4 -2]    ; k / g not positive
                   [16 1 6] [16 0 6] [16 -3 6]            ; locality below 2
                   [200 4 57] [255 2 2] [256 2 1]]        ; k + g past 256
          cases (into {} (map (fn [[k r g]] [(str "adm_" k "_" r "_" g)
                                             (str "(if (layout-admissible? "
                                                  k " " r " " g ") 1 0)")]))
                      configs)
          actual (run-cases cases)]
      (doseq [[k r g] configs]
        (let [cljc-ok? (try (lrc/layout {:k k :r r :g g}) true
                            (catch AssertionError _ false))]
          (is (= (if cljc-ok? 1 0) (get actual (str "adm_" k "_" r "_" g)))
              (str "layout-admissible? k=" k " r=" r " g=" g
                   " (.cljc " (if cljc-ok? "accepts" "throws") ")")))))))

;; --- the local drain ------------------------------------------------------

(def ^:private drain-corpus
  "The patterns above plus ones chosen for the drain specifically: a group
  missing two of its cover (never repairable), several independently
  repairable groups at once, a data shard alongside its own local parity, and
  a mix that has to fall through to a global solve."
  (into (vec erasure-corpus)
        [#{0 16} #{0 1} #{0 4 8 12 16} #{0 4 16 17} #{16 17 18 19}
         #{0 1 4 5 8 9} #{3 7 11 15} #{0 5 10 15 20 21 22}
         #{0 1 2 3 4 5 6 7} #{19 18} #{2 16 6 17}]))

(defn- leading-local-steps
  "How many `:local` steps the .cljc planner performs before it stops draining.
  Every local step retires exactly one missing shard, so the shards still
  missing at the fixpoint is the erased count minus this."
  [erased]
  (count (take-while #(= :local (:op %)) (:steps (lrc/recovery-plan lay erased)))))

(deftest local-repair-target-matches
  (testing "which shard a local repair rebuilds — the half of step 1 that had not crossed"
    (let [qs (range (:l lay))
          cases (into {}
                      (mapcat (fn [[i erased]]
                                (map (fn [q] [(str "tgt_" i "_" q)
                                              (str "(local-repair-target " K " " R " " q " "
                                                   (set-literal erased) ")")])
                                     qs)))
                      (map-indexed vector drain-corpus))
          actual (run-cases cases)]
      (doseq [[i erased] (map-indexed vector drain-corpus)
              q qs]
        ;; #'lrc/locally-repairable is the real .cljc decision — private, but
        ;; asserting against a reimplementation here would gate the test
        ;; against itself.
        (let [expected (or (first (#'lrc/locally-repairable lay q (set erased))) -1)]
          (is (= expected (get actual (str "tgt_" i "_" q)))
              (str "local-repair-target group " q " of " erased)))))))

(deftest local-drain-matches
  (testing "the fixpoint drain — step 1 of erasure.lrc/recovery-plan"
    (let [cases (into {}
                      (mapcat (fn [[i erased]]
                                (let [s (set-literal erased)]
                                  [[(str "steps_" i) (str "(local-drain-steps " K " " R " " s ")")]
                                   [(str "resid_" i) (str "(drain-residual " K " " R " " s ")")]
                                   [(str "full_" i)
                                    (str "(if (fully-local-repairable? " K " " R " " s ") 1 0)")]])))
                      (map-indexed vector drain-corpus))
          actual (run-cases cases)]
      (doseq [[i erased] (map-indexed vector drain-corpus)]
        (testing (str "pattern " erased)
          (let [plan (lrc/recovery-plan lay erased)
                locals (leading-local-steps erased)]
            (is (= locals (get actual (str "steps_" i))) "local-drain-steps")
            (is (= (- (count erased) locals) (get actual (str "resid_" i))) "drain-residual")
            (is (= (if (and (:recoverable? plan)
                            (every? #(= :local (:op %)) (:steps plan)))
                     1 0)
                   (get actual (str "full_" i)))
                "fully-local-repairable?"))))))
  (testing "the corpus actually exercises both sides of the decision"
    ;; A gate whose corpus only ever takes one branch is a gate that has never
    ;; been asked anything.
    (let [outcomes (map (fn [erased]
                          (let [plan (lrc/recovery-plan lay erased)]
                            (and (:recoverable? plan)
                                 (every? #(= :local (:op %)) (:steps plan)))))
                        drain-corpus)]
      (is (some true? outcomes) "some pattern is repaired entirely by xors")
      (is (some false? outcomes) "some pattern needs more than the local drain"))
    (is (some #(> (leading-local-steps %) 1) drain-corpus)
        "some pattern drains more than one group")))

(deftest drain-covers-disjoint-groups-in-one-pass
  (testing "an observation about erasure.lrc/recovery-plan, recorded not fixed"
    ;; `recovery-plan` says it repeats the local drain to a fixpoint \"because
    ;; rebuilding a group's data shard can make that group's local parity
    ;; rebuildable in the next round\". For THIS layout that cannot happen:
    ;; group q's cover is its data shards plus shard k+q, and those covers are
    ;; pairwise disjoint, so a repair in one group never changes another
    ;; group's missing count, and a group missing two of its own cover is
    ;; never repaired by any number of extra rounds. The loop is correct but
    ;; converges in one pass over the groups. The port mirrors the loop rather
    ;; than the shortcut, so this stays true if the covers ever overlap.
    (let [covers (map (fn [q] (conj (set (lrc/group-members lay q))
                                    (lrc/local-parity-of lay q)))
                      (range (:l lay)))]
      (doseq [[a b] (for [x (range (:l lay)) y (range (:l lay)) :when (< x y)] [x y])]
        (is (empty? (set/intersection (nth covers a) (nth covers b)))
            (str "covers of groups " a " and " b " are disjoint")))
      (doseq [erased drain-corpus]
        (is (= (leading-local-steps erased)
               (count (filter (fn [q]
                                (= 1 (count (filter (set erased)
                                                    (conj (set (lrc/group-members lay q))
                                                          (lrc/local-parity-of lay q))))))
                              (range (:l lay)))))
            (str "drain of " erased " needs no second pass"))))))

(deftest gopalan-bound-matches
  (let [configs [[16 4 6] [8 4 3] [12 3 4] [20 5 8]]
        cases (into {} (map (fn [[k r g]]
                              [(str "bound_" k "_" r "_" g)
                               (str "(max-tolerated-erasures " k " " r " " g ")")]))
                    configs)
        actual (run-cases cases)]
    (doseq [[k r g] configs]
      (is (= (lrc/max-tolerated-erasures (lrc/layout {:k k :r r :g g}))
             (get actual (str "bound_" k "_" r "_" g)))
          (str "bound for k=" k " r=" r " g=" g)))))

;; --- native-AOT admission -------------------------------------------------

(def ^:private word-typed-layer
  "The functions of the port that carry no set, no bool and no string: the
  whole field, the whole layout, and the whole generator matrix.

  These are the ones `kotoba.kir/only-native-word-typed-features?` — the
  admission gate for the x86_64 / aarch64 backends — accepts, so this layer is
  compilable to machine code with no interpreter and no host underneath it.
  That is worth pinning: it is a property of how the code is WRITTEN, and one
  set-typed parameter added to any of these silently takes it away."
  '#{gf-add gf-mul gf-pow gf-inv gf-div
     group-count shard-count group-of local-parity-of
     group-start group-end group-size role-of role-index-of
     global-parity-index cauchy-entry generator-entry
     local-repair-reads gopalan-distance-bound max-tolerated-erasures})

(defn- port-kir []
  (:kir (compiler/compile-source
         (str "(ns erasure-core (:export [gf-add]))\n" (strip-ns-form (slurp port-path)))
         :wasm32-kotoba-v1 {})))

(deftest word-typed-layer-is-native-aot-admissible
  (let [fns (:functions (port-kir))
        admits? (fn [f] (ir/only-native-word-typed-features? {:functions [f]}))
        by-name (into {} (map (juxt :name identity)) fns)]
    (testing "every declared word-typed function passes the native gate"
      (doseq [n word-typed-layer]
        (is (contains? by-name n) (str n " is missing from the port"))
        (when-let [f (get by-name n)]
          (is (admits? f) (str n " is no longer native-word-typed")))))
    (testing "so do the loop helpers the trampoline synthesises for them"
      ;; gf-mul and gf-pow lower to `__kotoba_loop_N`; if those were rejected
      ;; the exported names passing would mean nothing.
      (doseq [f fns :when (str/starts-with? (str (:name f)) "__kotoba_loop")]
        (is (admits? f) (str (:name f) " (synthesised) is not native-word-typed"))))))

(deftest set-typed-layer-is-not-native-aot-admissible
  (testing "and the module as a whole therefore is not"
    ;; Stated as a fact, not a defect. The gate is whole-module — it is
    ;; `(every? ... (:functions hir))` — so the repair decision, which needs a
    ;; set of erased indices, holds the field back. Splitting the module to buy
    ;; native admission for the field would trade a real property (one file
    ;; states the whole decision layer) for one no consumer has asked for yet.
    (let [kir (port-kir)
          admits? (fn [f] (ir/only-native-word-typed-features? {:functions [f]}))
          rejected (into #{} (comp (remove admits?) (map :name)) (:functions kir))]
      (is (false? (ir/only-native-word-typed-features? kir)))
      (is (contains? rejected 'first-repairable-group))
      (is (contains? rejected 'drain-steps-from))
      (testing "the two reasons, measured"
        ;; 1. a [:set :i64] parameter
        (is (every? (fn [n] (some #{[:set :i64]} (:param-types (get (into {} (map (juxt :name identity)) (:functions kir)) n))))
                    '[first-repairable-group drain-steps-from local-repair-target])
            "set-typed parameters")
        ;; 2. a :bool RESULT, independent of the body — layout-admissible? is
        ;; word-typed throughout and is still rejected, because this pin of
        ;; kotoba-kir requires the result type to be :i64 or :string. That is a
        ;; backend gap, not a reason to type a predicate as an integer.
        (is (contains? rejected 'layout-admissible?))
        (is (contains? rejected 'fully-local-repairable?))))))
