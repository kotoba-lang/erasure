(ns erasure.distance-test
  "What this construction's minimum distance ACTUALLY is.

  ADR-2607299200 section 1 turns on a number: how many arbitrary shard losses
  the k=16 / r=4 / g=6 code survives. The Gopalan et al. bound
  `d <= n - k - ceil(k/r) + 2` says an *optimal* LRC with these parameters
  could reach d = 8, i.e. tolerate 7. A bound is not a measurement, and a
  construction is free to fall short of it silently — so this namespace
  searches erasure patterns instead of citing the inequality.

  MEASURED RESULT (2026-07-29, exhaustive, recorded here so the number in the
  ADR has a provenance):

      k=16 r=4 g=6 n=26
      t=7   657,800 patterns   0 failures
      t=8 1,562,275 patterns   1,464 failures

  So the true minimum distance is exactly 8 and the code tolerates exactly 7
  arbitrary erasures — it MEETS the Gopalan bound, it does not merely approach
  it. The smallest failures are 8-shard patterns that wipe two whole local
  groups' worth of data (e.g. #{0..7}), which is the shape the bound predicts.

  The exhaustive t=7 sweep takes minutes, so it is not run on every `clj -M:test`.
  What runs here is: an exhaustive search on a small configuration where it is
  cheap, a bounded random sample at t=7 on the production configuration, and
  the specific 8-shard witnesses that must fail. `slow-distance-sweep` below
  reproduces the full numbers on demand."
  (:require [clojure.test :refer [deftest is testing]]
            [erasure.codec :as codec]
            [erasure.lrc :as lrc]))

(defn combinations
  "All `t`-subsets of `xs`, as vectors. Local so the library keeps zero deps."
  [xs t]
  (cond
    (zero? t) [[]]
    (< (count xs) t) []
    :else (let [[h & more] xs]
            (concat (map #(cons h %) (combinations more (dec t)))
                    (combinations more t)))))

(defn min-distance
  "Smallest number of erasures this layout cannot survive, found by exhaustive
  search. Returns `{:distance d :witness pattern}`."
  [lay]
  (first
   (keep (fn [t]
           (when-let [w (first (remove #(codec/decodable? lay (set %))
                                       (combinations (range (:n lay)) t)))]
             {:distance t :witness (vec w)}))
         (range 1 (inc (:n lay))))))

(deftest small-config-distance-is-exhaustively-verified
  (testing "k=8 r=4 g=3 -> l=2, n=13; Gopalan bound says d <= 13-8-2+2 = 5"
    (let [lay (lrc/layout {:k 8 :r 4 :g 3})]
      (is (= {:k 8 :r 4 :g 3 :l 2 :n 13} (select-keys lay [:k :r :g :l :n])))
      (is (= 4 (lrc/max-tolerated-erasures lay)) "the bound, tolerated form")
      (let [{:keys [distance witness]} (min-distance lay)]
        (is (= 5 distance)
            (str "measured minimum distance; first failing pattern " witness))
        (is (= 4 (dec distance))
            "so it tolerates 4 arbitrary erasures — meets the bound")))))

(deftest production-config-survives-every-sampled-seven
  (testing "k=16 r=4 g=6: a bounded sample of the 657,800 seven-subsets, all
            of which the recorded exhaustive sweep found recoverable"
    (let [lay (lrc/layout {:k 16 :r 4 :g 6})
          ;; Deterministic sample — a fixed stride through the ordered
          ;; subsets, not a random one, so a failure is reproducible.
          all (combinations (range 26) 7)
          sample (take-nth 37 all)
          failures (remove #(codec/decodable? lay (set %)) sample)]
      (is (= 7 (lrc/max-tolerated-erasures lay)))
      (is (empty? failures)
          (str "sampled " (count sample) " of " (count all)
               "; failures: " (take 3 failures))))))

(deftest production-config-fails-at-eight-where-expected
  (testing "the witnesses the exhaustive sweep found — two whole groups of
            data, which is exactly the shape the Gopalan bound predicts"
    (let [lay (lrc/layout {:k 16 :r 4 :g 6})]
      (doseq [w [#{0 1 2 3 4 5 6 7}
                 #{0 1 2 3 4 5 6 24}
                 #{0 1 2 3 4 5 6 25}]]
        (is (false? (codec/decodable? lay w))
            (str "expected unrecoverable: " w)))
      (is (= :rank-deficient
             (:reason (lrc/recovery-plan lay #{0 1 2 3 4 5 6 7})))
          "and reported as rank-deficient, not silently mis-decoded"))))

(defn ^:export slow-distance-sweep
  "Reproduce the recorded exhaustive numbers. Minutes, not seconds — call it
  by hand when the construction or the field changes, and update the table in
  this namespace's docstring and in ADR-2607299200 with what it prints."
  [k r g]
  (let [lay (lrc/layout {:k k :r r :g g})]
    (into {:config (select-keys lay [:k :r :g :l :n])
           :gopalan-tolerated (lrc/max-tolerated-erasures lay)}
          (map (fn [t]
                 (let [pats (combinations (range (:n lay)) t)
                       bad (remove #(codec/decodable? lay (set %)) pats)]
                   [t {:patterns (count pats) :failures (count bad)
                       :first-witnesses (mapv vec (take 3 bad))}])))
          (range 1 (inc (:n lay))))))
