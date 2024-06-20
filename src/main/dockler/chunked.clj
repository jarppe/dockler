(ns dockler.chunked
  (:require [dockler.data :as d])
  (:import (java.io InputStream
                    OutputStream)
           (java.nio ByteBuffer)))


(set! *warn-on-reflection* true)


(defn chunked-input-stream ^InputStream [^InputStream in]
  (let [chunk-left   (volatile! 0)
        available    (fn []
                       (vswap! chunk-left (fn [chunk-left]
                                            (if (pos? chunk-left)
                                              chunk-left
                                              (loop [len 0]
                                                (let [c (.read in)]
                                                  (if (= c (int \return))
                                                    (do (.read in) ; Read the \n 
                                                        ; If len = 0 it means this was the last chunk, so read the end-of-chunk marker
                                                        ; and return 0 indicating the end:
                                                        (when (zero? len)
                                                          (.read in)
                                                          (.read in))
                                                        len)
                                                    (recur (int (+ (* len 16)
                                                                   (- c (cond
                                                                          (<= (int \0) c (int \9)) (int \0)
                                                                          (<= (int \a) c (int \f)) (- (int \a) 10)
                                                                          (<= (int \A) c (int \F)) (- (int \A) 10)))))))))))))
        update-chunk (fn [bytes-read]
                       (vswap! chunk-left
                               (fn [chunk-left c]
                                 (let [chunk-left (- chunk-left c)]
                                   (when (zero? chunk-left)
                                                ; Read the end-of-chunk \r \n
                                     (.read in)
                                     (.read in))
                                   chunk-left))
                               bytes-read))]
    (proxy [InputStream] []
      (read
        ([]
         (if (= (available) 0)
           -1
           (let [c (.read in)]
             (update-chunk 1)
             c)))
        ([^bytes b] (.read ^InputStream this b 0 (alength b)))
        ([^bytes b off len] (let [a (available)]
                              (if (= a 0)
                                -1
                                (let [len (min a len)
                                      c   (.read in b off len)]
                                  (update-chunk c)
                                  c))))))))


(def ^:private ^"[B" CRLF (d/str->bytes "\r\n"))


(defn chunked-output-stream ^OutputStream [^OutputStream out]
  (let [buffer (ByteBuffer/allocate 256)
        flush  (fn []
                 (let [len (-> (.flip buffer)
                               (.remaining))]
                   (doto out
                     (.write (-> (Integer/toHexString len)
                                 (d/str->bytes)))
                     (.write CRLF)
                     (.write (.array buffer) 0 len)
                     (.write CRLF))
                   (.clear buffer)))
        write  (fn [data off len]
                 (let [in (java.io.ByteArrayInputStream. data off len)]
                   (loop [c (.read in)]
                     (when (not= c -1)
                       (when (zero? (.remaining buffer))
                         (flush))
                       (.put buffer (-> (int c)
                                        (Integer.)
                                        (.byteValue)))
                       (recur (.read in))))))]
    (proxy [OutputStream] []
      (write
        ([v]
         (if (bytes? v)
           (write v 0 (alength ^bytes v))
           (write (byte-array [(-> (int v) (Integer.) (.byteValue))]) 0 1)))
        ([v off len]
         (write v off len)))
      (close []
        (flush)
        (doto out
          (.write (int \0))
          (.write CRLF)
          (.write CRLF))))))
