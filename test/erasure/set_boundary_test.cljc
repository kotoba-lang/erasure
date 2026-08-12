(ns erasure.set-boundary-test
  "Why one decision did not move, as a test rather than a comment.

  `kotoba/erasure_core.kotoba` states the local-repair decision —
  `cover-missing`, `locally-repairable?`, `local-repair-target` and the drain —
  and `erasure.lrc` does not ask it. That is the single exception to the port,
  and an exception stated only in prose is one that quietly outlives its
  reason. So this runs the shipped artifact directly, on whichever runtime it
  is loaded under, and asserts what a `[:set :i64]` argument actually does.

  It is written to FAIL when the obstacle is removed. The day a compiler/kir
  pin lands where `:i64` set items compare under ClojureScript, the `:cljs`
  branch below stops throwing, this test goes red, and the message names the
  delegation that can then move. That is the opposite of a comment, which would
  simply become false.

  MEASURED 2026-08-12 at compiler `45f0e5e3` / kotoba-kir `fa2fbb58`:

    :clj   a set of 32 items crosses; 33 is refused (`typed-set-item-limit`)
    :cljs  a set of ONE item traps inside the guest —
           `kotoba.kir.value/compare-typed-values` sends `:i64` to
           `cljs.core/compare`, which cannot compare two `js/BigInt`s, and
           `typed-set-contains` compares. Scalar `:i64` exports are fine on the
           same runtime, in the same probe, which is why the rest of the port
           delegates on both."
  (:require [clojure.test :refer [deftest is testing]]
            [erasure.kotoba-oracle :as oracle]
            #?(:cljs [erasure.cljs-kir])))

(defn- i64s [xs] (mapv oracle/i64 (sort xs)))

(defn- typed-set [xs] [[:set :i64] (i64s xs)])

(defn- cover-missing
  "`erasure-core/cover-missing` for group `q` of a k=16 / r=4 layout, as a host
  integer — the guest answers a `js/BigInt` under `:cljs`."
  [q erased]
  (oracle/i64-value
   (oracle/call :erasure-core 'cover-missing
                [(oracle/i64 16) (oracle/i64 4) (oracle/i64 q) (typed-set erased)])))

(deftest scalar-exports-cross-on-this-runtime
  ;; The control. If these failed, nothing below would be evidence about sets.
  (is (= 76 (oracle/i64-value (oracle/call :erasure-core 'gf-mul
                                           [(oracle/i64 87) (oracle/i64 211)]))))
  (is (= 4 (oracle/i64-value (oracle/call :erasure-core 'local-repair-reads
                                          [(oracle/i64 16) (oracle/i64 4) (oracle/i64 1)]))))
  (is (true? (oracle/call :erasure-core 'layout-admissible?
                          [(oracle/i64 16) (oracle/i64 4) (oracle/i64 6)]))))

(deftest a-set-argument-is-what-it-is-measured-to-be
  #?(:clj
     (testing "on the JVM a set crosses, up to the interpreter's item limit"
       (is (= 0 (cover-missing 0 [])))
       (is (= 1 (cover-missing 1 [5])))
       (is (= 5 (cover-missing 1 [4 5 6 7 17])))
       (is (number? (cover-missing 0 (range 32))))
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not the declared typed set"
                             (cover-missing 0 (range 33)))
           "33 items is refused — kotoba.kir.value/typed-set-item-limit is 32"))

     :cljs
     (testing "under ClojureScript it does not, and that is why erasure.lrc
               still counts erasures itself"
       (is (= 0 (cover-missing 0 []))
           "the empty set is the one that crosses: nothing to compare")
       (doseq [erased [[5] [4 5] [4 5 6 7 17]]]
         (is (thrown-with-msg?
              js/Error #"Cannot compare"
              (cover-missing 1 erased))
             (str "a " (count erased) "-item [:set :i64] traps in the guest. If this"
                  " no longer throws, the pin has been fixed and"
                  " erasure.lrc/locally-repairable can ask"
                  " erasure-core/locally-repairable? instead of counting."))))))
