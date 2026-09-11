(ns erasure.kotoba-oracle-gen
  "Regenerate the shipped KIR from `kotoba/erasure_core.kotoba`.

      clojure -M:test:gen

  Runs under :test because the compiler lives there and must not reach the
  library. What it writes IS what a consumer loads, so nothing here transforms
  it: same compile call as the drift test, pretty-printed EDN, no
  post-processing. If this file and that test disagreed about how to compile,
  the test would be checking something other than what ships.

  Unlike `erasure.kotoba-parity-test`, this compiles the file AS IT STANDS —
  the parity harness strips the port's own `ns` form so it can add probe
  functions, and a shipped artifact must be the source, not a variant of it."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [erasure.kotoba-oracle :as oracle]
            [kotoba.compiler.core :as compiler])
  (:gen-class))

(def target
  "The portable target the shipped KIR is compiled for.

  KIR is target-independent for this core, but ONE of them has to be the
  artifact, and naming it here rather than in two places is what keeps
  regeneration reproducible. `:wasm32-kotoba-v1` is the target
  `erasure.kotoba-parity-test` has always compiled the port for."
  :wasm32-kotoba-v1)

(defn compile-kir [source-path]
  (let [result (compiler/compile-source (slurp (io/file source-path)) target {})]
    (or (:kir result)
        (throw (ex-info "compile-source returned no :kir" {:source source-path})))))

(defn write-artifact! [id source-path]
  (let [out (io/file "resources" (oracle/resource-path id))]
    (io/make-parents out)
    (spit out (with-out-str (pp/pprint (compile-kir source-path))))
    (.getPath out)))

(defn regenerate-all! []
  (mapv (fn [[id source]] (write-artifact! id source)) (sort-by key oracle/cores)))

(defn -main [& _]
  (run! println (regenerate-all!))
  (shutdown-agents))
