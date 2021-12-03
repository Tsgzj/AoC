(ns day3
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def read-input
  (map (fn [s] (map #(Character/getNumericValue %) s)) (get-lines "input/day3")))

(defn bin-to-dec [b-lst res carry]
  (cond (empty? b-lst) res
        :else (bin-to-dec (drop-last b-lst) (+ res (* carry (last b-lst))) (* 2 carry))))

(defn to-dec [b-lst]
  (bin-to-dec b-lst 0 1))

(defn binary-diagno [input]
  (let [cnt (count input)
        mjr (/ cnt 2)
        dig (count (first input))]
    (->> (apply map vector input)
         (map #(reduce + %))
         (map #(cond
                 (> % mjr) 1
                 :else 0))
         (to-dec)
         (#(*' % (-' (reduce *' (repeat dig 2)) 1 %))))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (binary-diagno read-input))))
