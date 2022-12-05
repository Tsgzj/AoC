(ns day1
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def read-input
  (map #(Integer/parseInt %) (get-lines "input/day1")))

(defn prepare-n-sum [n lst]
  (->> (partition n 1 lst)
       (map #(reduce + %))))

(defn sonar-sweep [n lst]
  (let [zip (fn [lhs rhs]
              (map vector lhs rhs))
        diff (fn [lst]
               (zip (drop-last lst) (drop 1 lst)))]
    (->> (prepare-n-sum n lst)
         (diff)
         (map #(- (last %) (first %)))
         (filter #(> % 0))
         (count))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (sonar-sweep 1 read-input)))
  (pp/pprint (format "Problem two: %d" (sonar-sweep 3 read-input))))
