(ns erasure.cljs-kir
  "Hands `erasure.kotoba-oracle` the shipped artifact on a runtime that cannot
  read it itself, AT LOAD TIME.

  Load time, not `-main`, and that is the whole reason this is a namespace of
  its own rather than three lines in `erasure.cljs-runner`: `erasure.codec-test`
  builds a layout in a top-level `def`, so the artifact has to be registered
  before that namespace is loaded, not before the tests are run. Requiring this
  ahead of the test namespaces is what orders the two.

  It also states, in the place a ClojureScript reader will look, the narrowing
  that came with the port: this library used to be loadable on any cljs host
  with nothing but its own source, and now a host must get the artifact to
  `register-kir!` first. A node host reads it off disk; a browser host would
  inline it at build time or fetch it. What it must NOT do is compute the
  layout itself — that is the second implementation the port exists to remove."
  ;; `oracle` is used only from the `:cljs` branch below — under `:clj` this
  ;; namespace does nothing, because `erasure.kotoba-oracle` reads the same
  ;; files off the classpath by itself.
  (:require ^{:clj-kondo/ignore [:unused-namespace]}
            [erasure.kotoba-oracle :as oracle]
            #?(:cljs [cljs.reader :as reader])))

(defn register-shipped-kir!
  "Register every declared core from `resources/` via node's `fs`.

  No-op under `:clj`, where `erasure.kotoba-oracle` reads the same files off
  the classpath."
  []
  #?(:cljs
     (let [fs (js/require "fs")]
       (doseq [id (keys oracle/cores)]
         (oracle/register-kir!
          id
          (reader/read-string
           (.readFileSync fs (str "resources/" (oracle/resource-path id)) "utf8")))))
     :clj nil))

#?(:cljs (register-shipped-kir!))
