(ns dockler.stream
  (:require [dockler.pipe :as pipe])
  (:import (java.io InputStream
                    OutputStream)))


;;
;; Utils for streaming I/O from/to containers:
;;


(defn- close! [^java.io.Closeable s]
  (when s
    (try (.close s) (catch Exception _))))


(defn eof! []
  (throw (ex-info "unexpected EOF" {})))


(defn read! [^InputStream in]
  (let [c (.read in)]
    (when (= c -1) (eof!))
    c))


(defn- read-frame [^InputStream in]
  (let [t (.read in)]
    (when (not= t -1)
      (read! in)
      (read! in)
      (read! in)
      (let [b3         (read! in)
            b2         (read! in)
            b1         (read! in)
            b0         (read! in)
            stream-len (bit-or (bit-and (bit-shift-left b3 12) 0xff000000)
                               (bit-and (bit-shift-left b2 8)  0x00ff0000)
                               (bit-and (bit-shift-left b1 4)  0x0000ff00)
                               (bit-and b0                     0x000000ff))
            message    (.readNBytes in stream-len)
            stream     (case t
                         1 :stdout
                         2 :stderr)]
        (when-not (= stream-len (alength message))
          (eof!))
        [stream message]))))


(defn- stream-frames [^InputStream in stdout stderr]
  (future
    (try
      (let [nop     (constantly nil)
            streams {:stdout (or stdout nop)
                     :stderr (or stderr nop)}]
        (loop []
          (when (Thread/interrupted) (throw (InterruptedException.)))
          (when-let [[stream message] (read-frame in)]
            ((streams stream) message)
            (recur))))
      (catch java.nio.channels.ClosedByInterruptException _)
      (catch Exception e
        (.println System/err (str (-> e .getClass .getName) ": " (-> e .getMessage)))
        (.printStackTrace e System/err))
      (finally
        (when stdout (stdout))
        (when stderr (stderr))
        (close! in)))))


(defrecord StreamResp [^OutputStream stdin ^InputStream stdout ^InputStream stderr ^java.io.Closeable -conn -streaming]
  java.io.Closeable
  (close [_]
    (when -streaming (future-cancel -streaming))
    (close! stdin)
    (close! stdout)
    (close! stderr)
    (close! -conn)))


(defn stream-resp [resp opts]
  (when-not (-> resp :headers (get "content-type") (= "application/vnd.docker.multiplexed-stream"))
    (throw (ex-info (str "expected content-type \"application/vnd.docker.multiplexed-stream\", got "
                         (-> resp :headers (get "content-type") (pr-str)))
                    {:resp resp})))
  (let [stdout    (when (:stdout opts) (pipe/pipe))
        stderr    (case (:stderr opts)
                    true (pipe/pipe)
                    :stdout stdout
                    nil)
        streaming (when (or stdout stderr)
                    (stream-frames (:in resp) stdout stderr))]
    (map->StreamResp {:stdin      (when (:stdin opts) (:out resp))
                      :stdout     stdout
                      :stderr     stderr
                      :-conn      (:conn resp)
                      :-streaming streaming})))

