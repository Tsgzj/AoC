(ns day8
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def input
  (letfn [(parse-part [p] (str/split (str/trim p) #" "))
          (parse-line [l] (map parse-part (str/split l #"\|")))]
    (->> (get-lines "input/day8")
         (map parse-line))))

(defn unique-seg [m]
  (map #(get m %) '(2 3 4 7)))

(def segmentSearch1
  (let [unique-seg
        (fn [m]
          ( map #(get m %) '(2 3 4 7)))]
    (->> (mapcat last input)
         (map count)
         frequencies
         unique-seg
         (reduce +))))

;; 11111
;; 2   3
;; 2   3
;; 44444
;; 5   6
;; 5   6
;; 77777

(def seg
  {[:3 :6]                1
   [:1 :3 :4 :5 :7]       2
   [:1 :3 :4 :6 :7]       3
   [:2 :3 :4 :6]          4
   [:1 :2 :4 :6 :7]       5
   [:1 :2 :4 :5 :6 :7]    6
   [:1 :3 :4]             7
   [:1 :2 :3 :4 :5 :6 :7] 8
   [:1 :2 :3 :4 :6 :7]    9})
