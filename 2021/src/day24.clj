(ns day24
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn parse-line [line]
  (let [num? (fn [s] (every? #(Character/isDigit %) s))]
    (->> (str/split line #" ")
         (map (fn [s]
                (if (num? s)
                  (Integer/parseInt s)
                  (keyword s)))))))

(def input
  (map parse-line (get-lines "input/day24")))

(def env (atom {}))

(defn eval-val [x]
  (if (number? x)
    x
    (x @env)))

(defmulti alu-op first)

(defmethod alu-op :inp [_ a]
  (swap! env {a }))

(defmethod alu-op :add [_ a b]
  )
