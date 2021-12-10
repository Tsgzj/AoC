(ns day9
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def input
  (parse-input (get-lines "input/day9")))


(defn parse-input [lines]
  (into {} (for [[m line] (map-indexed vector lines)
                 [n h]    (map-indexed vector line)]
             [[m n] (Character/digit h 10)])))

(defn neighbours [m [i j]]
  (filter #(m %)
          [[(inc i) j] [(dec i) j] [i (inc j)] [i (dec j)]]))

(defn lowest? [m [p v]]
  (every? #(> (m %) v) (neighbours m p)))

(def risk-level
  (->> input
       (filter #(lowest? input %))
       (map #(inc (second %)))
       (reduce +)))

(defn find-basin-helper [m p visited]
  (cond (= (m p) 9) 0
        (@visited p) 0
        :else (do (swap! visited conj p)
                  (reduce + 1 (map #(find-basin-helper m % visited)
                                   (neighbours m p))))))

(defn find-basin [m [p v]]
  (let [visited (atom #{})]
    (find-basin-helper m p visited)))

(def smoke-basin
  (->> input
       (filter #(lowest? input %))
       (map #(find-basin input %))
       sort
       reverse
       (take 3)
       (reduce *)))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" risk-level))
  (pp/pprint (format "Problem two: %d" smoke-basin)))
