(ns erasure.gf-test
  (:require [clojure.test :refer [deftest is testing]]
            [erasure.gf :as gf]))

(deftest table-multiply-matches-the-definition
  (testing "all 65,536 pairs — the tables are an optimisation, not a variant"
    (is (every? (fn [a]
                  (every? (fn [b] (= (gf/mul-def a b) (gf/mul a b)))
                          (range 256)))
                (range 256)))))

(deftest field-axioms
  (testing "additive identity and self-inverse"
    (is (every? #(= % (gf/add % 0)) (range 256)))
    (is (every? #(zero? (gf/add % %)) (range 256))))

  (testing "multiplicative identity and absorbing zero"
    (is (every? #(= % (gf/mul % 1)) (range 256)))
    (is (every? #(zero? (gf/mul % 0)) (range 256))))

  (testing "every non-zero element has an inverse"
    (is (every? #(= 1 (gf/mul % (gf/inv %))) (range 1 256))))

  (testing "commutativity"
    (is (every? (fn [a] (every? (fn [b] (= (gf/mul a b) (gf/mul b a)))
                                (range 256)))
                (range 256))))

  (testing "associativity and distributivity on a spread"
    (let [xs (range 0 256 7)]
      (is (every? (fn [[a b c]]
                    (and (= (gf/mul a (gf/mul b c)) (gf/mul (gf/mul a b) c))
                         (= (gf/mul a (gf/add b c))
                            (gf/add (gf/mul a b) (gf/mul a c)))))
                  (for [a xs b xs c xs] [a b c]))))))

(deftest alpha-is-primitive
  (testing "2 generates the whole multiplicative group under 0x11D — this is
            why the log tables can be built by doubling, and why the field is
            0x11D and not the AES 0x11B (where 2 has order 51)"
    (is (= 255 (count (set (take 255 (iterate #(gf/mul % 2) 1))))))
    (is (= 1 (gf/pow 2 255)))))

(deftest pow-agrees-with-repeated-multiplication
  (is (every? (fn [a]
                (every? (fn [e]
                          (= (gf/pow a e)
                             (reduce (fn [acc _] (gf/mul acc a)) 1 (range e))))
                        (range 0 12)))
              (range 0 256 13))))

(deftest inverse-via-pow-254-agrees
  (testing "the identity the .kotoba port relies on: a^254 = a^-1"
    (is (every? #(= (gf/inv %) (gf/pow % 254)) (range 1 256)))))

(deftest scale-add-is-the-linear-combination-it-claims
  (let [xs [1 2 3 4 250 251]
        acc [10 20 30 40 50 60]]
    (is (= acc (gf/scale-add acc 0 xs)) "zero coefficient is a no-op")
    (is (= (mapv bit-xor acc xs) (gf/scale-add acc 1 xs)))
    (is (= (mapv (fn [a x] (bit-xor a (gf/mul 7 x))) acc xs)
           (gf/scale-add acc 7 xs)))))
