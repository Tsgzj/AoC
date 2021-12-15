(ns day14
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]) )

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def file (get-lines "input/day14"))

(def rules
  (let [rules-raw (rest (rest file))
        parse-line (fn [line] (map #(str/trim %) (str/split line #"->")))
        parse-rule (fn [pattern outcome]
                     [(vec pattern) (vec (str (subs pattern 0 1) outcome (subs pattern 1)))])]
    (into {}
          (map #(apply parse-rule %) (map parse-line rules-raw)))))

(defn build-pair [s]
  (frequencies
   (conj (map vec (partition 2 1 s))
         [(first s)]
         [(last s)])))

(def input (build-pair (first file)))

(defn re-pair [[k v]]
  (if (> (count k) 2)
    (map #(hash-map % v) (map vec (partition 2 1 k)))
    [(hash-map k v)]))

(defn insert [rules pairs]
  (into {}
        (map (fn [[k v]]
               {(get rules k k) v}) pairs)))

(defn pair-insert [rules pairs]
  (reduce #(merge-with + %1 %2)
          {}
          (mapcat re-pair (insert rules pairs))))

(defn calculate [step]
  (let [diff (fn [freq] (- (apply max freq) (apply min freq)))
        half (fn [n] (/ n 2))]
    (->> (iterate #(pair-insert rules %) input)
         (take (inc step))
         last
         flatten
         frequencies
         vals
         diff
         half)))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" ))
  (pp/pprint (format "Problem two: \n")))
