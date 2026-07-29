(ns erasure.cljs-runner
  "Run the portable suite under a real ClojureScript host (cljs.main
  --target node) — the fleet runtime priority puts cljs ahead of the JVM.

    clojure -Sdeps '{:paths [\"src\" \"test\"]}' -M:cljs \\
      -m cljs.main --target node -m erasure.cljs-runner

  `erasure.kotoba-parity-test` is deliberately absent: it needs
  kotoba-lang/compiler, which is JVM-only, so the parity gate runs on the
  compat side. Everything the gate covers on the `.kotoba` side has a `.cljc`
  counterpart here, so this run still exercises the whole library."
  (:require [clojure.test :as t :refer [run-tests]]
            [erasure.codec-test]
            [erasure.distance-test]
            [erasure.gf-test]))

#?(:cljs
   (defmethod t/report [:cljs.test/default :end-run-tests] [m]
     (when-not (t/successful? m)
       (set! (.-exitCode js/process) 1))))

(defn -main []
  (run-tests 'erasure.gf-test 'erasure.codec-test 'erasure.distance-test))
