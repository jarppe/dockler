(ns dockler.content-stream
  (:require [dockler.data :as d])
  (:import (java.io InputStream
                    OutputStream)
           (java.nio ByteBuffer)))


(set! *warn-on-reflection* true)


(def ^:const CR (int \return))
(def ^:const NL (int \newline))
(def ^"[B" CRLF (byte-array [CR NL]))

(def ^:const CH0 (int \0))
(def ^:const CH9 (int \9))
(def ^:const CHa (int \a))
(def ^:const CHf (int \f))
(def ^:const CHA (int \A))
(def ^:const CHF (int \F))
(def ^:const a-10 (- CHa 10))
(def ^:const A-10 (- CHA 10))


;;
;; ===============================================================================
;; Utils:
;; ===============================================================================
;;


(defn ascii->hex ^long [^long c]
  (- c (cond
         (<= CH0 c CH9) CH0
         (<= CHa c CHf) a-10
         (<= CHA c CHF) A-10
         :else (throw (java.io.IOException (format "unexpected input: expected chunk-len hex, read: 0x%02X" c))))))


(defn read!
  (^long [^java.io.InputStream in]
   (let [c (.read in)]
     (if (= c -1)
       (throw (java.io.EOFException.))
       (long c))))
  (^long [^java.io.InputStream in expected]
   (let [c (.read in)]
     (cond
       (= c -1) (throw (java.io.EOFException.))
       (not= c expected) (throw (java.io.IOException (format "unexpected input: expected: 0x%02X, read: 0x%02X" expected c)))
       :else (long c)))))


;;
;; ===============================================================================
;; Chunked input stream:
;; ===============================================================================
;;


;; Sometimes the docker daemon returns an extra five bytes at the end of chunked body. 
;; You can verify this with curl:
;;
;;    $ curl -v --unix-socket /var/run/docker.sock http://localhost/v1.46/version
;;    ...normal curl output...
;;    * Leftovers after chunking: 5 bytes 
;;
;; The `Leftovers after chunking: 5 bytes` message denotes the fact that the response had a
;; body with valid chunked encoding content, but the response also contained 5 extra bytes.
;;
;; The extra bytes are always the same: `0\r\n\r\n`. It looks as the daemon responds with two
;; bodies, each encoded as chunked. The first is the actual response payload, and the second is
;; an empty body (empty body is encoded as `0\r\n\r\n`).
;;
;; This implementation reads the input stream after the body up to 5 bytes. If the 5 bytes were
;; those extra bytes it discards them. If not, the read bytes are unread by pushing them back to
;; input stream for later processing (the input stream must be a java.io.PushbackInputStream).
;;


(def extra-content (map int "0\r\n\r\n"))


(defn maybe-discard-extra-bytes ^java.io.PushbackInputStream [^java.io.PushbackInputStream in]
  (let [pushback (loop [[expected & more] extra-content
                        excess            []]
                   (if expected
                     (let [c (.read in)]
                       (cond
                         (= c expected) (recur more (conj excess c))
                         (= c -1) excess
                         :else (conj excess c)))
                     nil))]
    (when (seq pushback)
      (.unread in (byte-array pushback)))
    in))


(defn read-chunk-len [^java.io.InputStream in]
  (loop [chunk-len (ascii->hex (read! in))]
    (let [c (read! in)]
      (if (= c CR)
        (do (read! in NL)
            (when (zero? chunk-len)
              (read! in CR)
              (read! in NL)
              (when (pos? (.available in))
                (maybe-discard-extra-bytes in)))
            chunk-len)
        (recur (long (+ (* chunk-len 16)
                        (ascii->hex c))))))))


(defn chunked-input-stream ^InputStream [^InputStream in]
  (let [chunk-left   (volatile! (read-chunk-len in))
        update-chunk (fn [bytes-read]
                       (vswap! chunk-left (fn [chunk-left bytes-read]
                                            (let [chunk-left (- chunk-left bytes-read)]
                                              (if (zero? chunk-left)
                                                (do (read! in CR)
                                                    (read! in NL)
                                                    (read-chunk-len in))
                                                chunk-left)))
                               bytes-read))]
    (proxy [InputStream] []
      (read
        ([]
         (if (= @chunk-left 0)
           -1
           (let [c (read! in)]
             (update-chunk 1)
             c)))
        ([^bytes b] (.read ^InputStream this b 0 (alength b)))
        ([^bytes b off len] (let [a @chunk-left]
                              (if (= a 0)
                                -1
                                (let [len (min a len)
                                      c   (.read in b off len)]
                                  (update-chunk c)
                                  c))))))))


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