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
  (:require [clojure.string :as str]
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
  (let [xs (concat [0 1 2 255] (range 3 256 17))
        cases (into {} (map-indexed (fn [i a] [(str "inv_" i) (str "(gf-inv " a ")")]) xs))
        actual (run-cases cases)]
    (doseq [[i a] (map-indexed vector xs)]
      (is (= (gf/inv a) (get actual (str "inv_" i))) (str "gf-inv " a)))))

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
