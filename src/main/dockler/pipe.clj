(ns dockler.pipe
  (:import (java.nio ByteBuffer)))


(set! *warn-on-reflection* true)


;; Returns a pipe. Pipe implements InputStream and a function with zero and one arities. 
;; When called with one argument it accepts a byte array and it schedules the data for
;; consuption via the InputStream API. When called without arguments it signals the end
;; of data.

(defn pipe []
  (let [buffers   (java.util.concurrent.LinkedBlockingDeque. 256)
        buffer    (volatile! (ByteBuffer/allocate 0))
        open?     (volatile! true)
        available (fn []
                    (if-not @open?
                      -1
                      (let [remaining (.remaining ^ByteBuffer @buffer)]
                        (if (> remaining 0)
                          remaining
                          (let [next-buffer (.take buffers)]
                            (if (= next-buffer ::EOF)
                              (do (vreset! open? false)
                                  -1)
                              (do (vreset! buffer next-buffer)
                                  (.remaining ^ByteBuffer next-buffer))))))))]
    (proxy [java.io.InputStream clojure.lang.IFn] []
      ;;
      ;; java.io.InputStream
      ;;
      (read
       ([]
        (if (= (available) -1)
          -1
          (-> (.get ^ByteBuffer @buffer) (int) (bit-and 0xff))))
       ([^bytes b] (.read ^java.io.InputStream this b 0 (alength b)))
       ([^bytes b off len] (let [remaining (available)]
                             (if (= remaining -1)
                               -1
                               (let [len (min remaining len)]
                                 (.get ^ByteBuffer @buffer b off len)
                                 len)))))
      (close [] (vreset! open? false))
      ;;
      ;; clojure.lang.IFn
      ;;
      (invoke
        ([] (.put buffers ::EOF) @open?)
        ([^bytes data] (if @open?
                         (do (when (> (alength data) 0)
                               (.put buffers (ByteBuffer/wrap data)))
                             true)
                         false))))))

