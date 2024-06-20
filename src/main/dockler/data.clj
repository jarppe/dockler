(ns dockler.data
  (:require [clojure.string :as str]
            [clojure.walk :as walk])
  (:import (java.nio.charset StandardCharsets)))


(set! *warn-on-reflection* true)


(defn str->bytes ^"[B" [^String v]
  (.getBytes v StandardCharsets/UTF_8))


;;
;; Mapping data from Go to Clj and back:
;;


(defn clj->go-kw [k]
  (-> k
      (name)
      (str/replace #"^([^-])" (fn [[_ a]] (str/upper-case a)))
      (str/replace #"([^-])\-([^-])" (fn [[_ a b]] (str a (str/upper-case b))))
      (keyword)))


(defn clj->go [data]
  (walk/postwalk (fn [v]
                   (if (keyword? v)
                     (clj->go-kw v)
                     v))
                 data))


(defn go->clj-kw [k]
  (-> k
      (str/replace #"([^A-Z])([A-Z]+)" (fn [[_ a b]] (str a "-" (str/lower-case b))))
      (str/replace #"^([A-Z]+)" (fn [[_ a]] (str/lower-case a)))
      (keyword)))


(defn go->clj [data]
  (cond
    (map? data) (reduce-kv (fn [acc k v]
                             (case k
                               "Containers" (assoc acc :containers (cond
                                                                     (map? v) (update-vals v go->clj)
                                                                     (sequential? v) (mapv go->clj v)
                                                                     :else v))
                               "Labels" (assoc acc :labels v)
                               (assoc acc (go->clj-kw k) (go->clj v))))
                           {}
                           data)
    (sequential? data) (mapv go->clj data)
    :else data))
