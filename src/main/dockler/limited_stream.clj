(ns dockler.limited-stream
  (:import (java.io InputStream)))


(set! *warn-on-reflection* true)


(defn content-length-input-stream ^InputStream [^InputStream in content-length]
  (let [available (volatile! content-length)]
    (proxy [InputStream] []
      (read
        ([] (if (= @available 0)
              -1
              (do (vswap! available dec)
                  (.read in))))
        ([^bytes b] (.read ^InputStream this b 0 (alength b)))
        ([^bytes b off len] (let [len (min @available len)]
                              (if (= len 0)
                                -1
                                (let [c (.read in b off len)]
                                  (vswap! available - c)
                                  c))))))))