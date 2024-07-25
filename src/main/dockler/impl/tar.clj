(ns dockler.impl.tar
  (:require [clojure.java.io :as io])
  (:import (java.io OutputStream)))


; The Apache commons has support for Tar, but that would not work with bb. Consider
; implementing dynamic support for Tar stream and use this as fallback.


(set! *warn-on-reflection* true)


(defn tar [^OutputStream out working-dir paths]
  (let [p (-> (ProcessBuilder. ^java.util.List (concat ["tar" "czf" "-"] paths))
              (.directory (io/file working-dir))
              (.start))]
    (io/copy (.getInputStream p) out)
    (when-not (.waitFor p 100 java.util.concurrent.TimeUnit/MILLISECONDS)
      (throw (ex-info "tar refuses to exit" {})))
    (let [exit (.exitValue p)]
      (when-not (= exit 0)
        (throw (ex-info (str "tar exited with code " exit) {:exit exit}))))
    true))


(comment
  (with-open [out (-> (io/file "out.tar.gz")
                      (io/output-stream))]
    (tar out "foo" ["a" "b/c"]))
  ;
  )