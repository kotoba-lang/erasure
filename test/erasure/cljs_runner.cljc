(ns erasure.cljs-runner
  "Run the portable suite under a real ClojureScript host (cljs.main
  --target node) — the fleet runtime priority puts cljs ahead of the JVM.

    clojure -Sdeps '{:paths [\"src\" \"test\"]}' -M:cljs \\
      -m cljs.main --target node -m erasure.cljs-runner

  `erasure.kotoba-parity-test`, `erasure.kotoba-oracle-test` and
  `erasure.kotoba-oracle-gen` are deliberately absent: they need
  kotoba-lang/compiler, which is JVM-only, so the parity, drift and delegation
  gates run on the compat side. Everything they cover on the `.kotoba` side has
  a `.cljc` counterpart here, so this run still exercises the whole library.

  ## What a ClojureScript host has to do now, and did not before

  `erasure.lrc` and `erasure.matrix` run the shipped decision core, and
  `erasure.kotoba-oracle` reads it from the classpath — which this runtime does
  not have. `erasure.cljs-kir`, required first and deliberately, hands it in.
  Without that, building a layout throws `no classpath on this runtime —
  register-kir! first`. That is a real narrowing, stated rather than
  discovered, and the price of the rules having one home.

  ## And why running it matters

  This is the only place the shipped artifact is exercised under `:cljs` reader
  conditionals — where an `:i64` is a `js/BigInt` and not a JVM long. That is
  not a formality: a sibling slice measured a `kotoba.kir` bug at these pins
  that only fires under ClojureScript, in cores that format integers into
  strings. This core is `:i64` in and `:i64`/`:bool` out with no string
  anywhere, so it should not reach it — and this runner is how that is checked
  rather than assumed."
  (:require [clojure.test :as t :refer [run-tests]]
            ;; FIRST, and load-bearing: `erasure.codec-test` builds a layout in
            ;; a top-level `def`, which reaches the oracle while it is being
            ;; loaded. See that namespace for why this is not in `-main`.
            [erasure.cljs-kir]
            [erasure.codec-test]
            [erasure.distance-test]
            [erasure.gf-test]
            [erasure.set-boundary-test]))

#?(:cljs
   (defmethod t/report [:cljs.test/default :end-run-tests] [m]
     (when-not (t/successful? m)
       (set! (.-exitCode js/process) 1))))

(defn -main []
  (run-tests 'erasure.gf-test 'erasure.codec-test 'erasure.distance-test
             'erasure.set-boundary-test))
