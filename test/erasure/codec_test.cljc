(ns erasure.codec-test
  (:require [clojure.test :refer [deftest is testing]]
            [erasure.codec :as codec]
            [erasure.gf :as gf]
            [erasure.lrc :as lrc]
            [erasure.matrix :as matrix]))

(def lay (lrc/layout {:k 16 :r 4 :g 6}))

(defn- shard [seed len]
  (mapv #(mod (+ (* seed 31) (* % 17) (* seed % 3)) 256) (range len)))

(def data (mapv #(shard % 24) (range 16)))
(def encoded (codec/encode lay data))

(defn- erase [idxs]
  (reduce #(assoc %1 %2 nil) encoded idxs))

(deftest layout-is-what-the-adr-specifies
  (is (= {:k 16 :r 4 :g 6 :l 4 :n 26}
         (select-keys lay [:k :r :g :l :n])))
  (is (= 26 (count encoded)))
  (is (= 1.625 (double (/ (:n lay) (:k lay))))
      "storage multiplier — ADR-2607299200 section 1"))

(deftest code-is-systematic
  (testing "data shards pass through unchanged, which is what lets a Range GET
            read one shard directly instead of reconstructing a stripe"
    (is (= data (subvec encoded 0 16)))))

(deftest local-parity-is-plain-xor
  (doseq [q (range (:l lay))]
    (is (= (gf/xor-shards (map #(nth data %) (lrc/group-members lay q)) 24)
           (nth encoded (lrc/local-parity-of lay q)))
        (str "local parity " q " is the xor of its group"))))

(deftest single-shard-loss-is-a-local-repair
  (testing "the central claim of the code: one lost shard costs r reads"
    (doseq [i (range (:n lay))]
      (let [plan (lrc/recovery-plan lay #{i})]
        (is (:recoverable? plan))
        (when (< i 20)                       ; data or local parity
          (is (= 1 (count (:steps plan))))
          (is (= :local (:op (first (:steps plan)))))
          (is (= 4 (count (:reads plan)))
              (str "shard " i " repaired from 4 reads")))
        (is (= encoded (codec/decode lay (erase #{i}))))))))

(deftest whole-group-loss-round-trips
  (doseq [q (range (:l lay))]
    (let [members (conj (set (lrc/group-members lay q)) (lrc/local-parity-of lay q))]
      (is (= encoded (codec/decode lay (erase members)))
          (str "group " q " plus its local parity, " (count members) " shards")))))

(deftest global-solve-round-trips
  (testing "patterns no local group can finish"
    (doseq [pattern [#{0 1 2 3 4 5}
                     #{0 4 8 12 1 5}
                     #{0 1 2 3 4 5 6}
                     #{20 21 22 23 24 25}
                     #{16 17 18 19}
                     #{0 1 16 4 5 17 8}]]
      (is (= encoded (codec/decode lay (erase pattern)))
          (str "erasing " pattern)))))

(deftest plan-read-set-is-sufficient-on-its-own
  (testing "a caller can fetch lazily — the plan's read set alone rebuilds the
            target, so a repair never has to pull the whole stripe"
    (let [plan (lrc/recovery-plan lay #{7})
          reads (:reads plan)]
      (is (= 4 (count reads)))
      (is (= (nth encoded 7)
             (gf/xor-shards (map #(nth encoded %) reads) 24))
          "target is the xor of exactly the shards the plan names"))))

(deftest unrecoverable-patterns-are-reported-not-guessed
  (testing "beyond the code, decode returns nil rather than a wrong shard"
    (let [dead #{0 1 2 3 4 5 6 7}]
      (is (false? (:recoverable? (lrc/recovery-plan lay dead))))
      (is (nil? (codec/decode lay (erase dead)))))))

(deftest generator-rows-reproduce-every-shard
  (testing "each shard really is its generator row applied to the data —
            including local parities, which is what licenses using them as
            equations in a global solve"
    (doseq [i (range (:n lay))]
      (is (= (nth encoded i)
             (matrix/apply-row (lrc/generator-row lay i) data 24))
          (str "shard " i)))))

(deftest matrix-inverse-round-trips
  (testing "a Cauchy block inverts and composes back to the identity"
    (let [rows (mapv #(lrc/generator-row lay %) (range 20 26))
          sub (mapv #(vec (take 6 %)) rows)
          inv (matrix/invert sub)]
      (is (some? inv))
      (doseq [i (range 6) j (range 6)]
        (is (= (if (= i j) 1 0)
               (reduce bit-xor 0 (map #(gf/mul (nth (nth sub i) %) (nth (nth inv %) j))
                                      (range 6))))
            (str "product entry " i "," j))))))

(deftest odd-shaped-layouts-work
  (testing "k not divisible by r leaves a short last group"
    (let [lay (lrc/layout {:k 10 :r 4 :g 3})
          data (mapv #(shard % 8) (range 10))
          enc (codec/encode lay data)]
      (is (= {:k 10 :r 4 :g 3 :l 3 :n 16} (select-keys lay [:k :r :g :l :n])))
      (is (= [8 9] (lrc/group-members lay 2)) "last group holds 2, not 4")
      (is (= 2 (lrc/local-repair-reads-for lay 2)))
      (doseq [i (range (:n lay))]
        (is (= enc (codec/decode lay (reduce #(assoc %1 %2 nil) enc #{i})))
            (str "single loss of shard " i))))))
