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

(defn bit-criteria [f input n]
  (let [cnt (count input)
        mjr (/ cnt 2)
        sum (reduce + (map #(nth % n) input))]
    (cond (f sum mjr) (filter #(= 1 (nth % n)) input)
          :else (filter #(= 0 (nth % n)) input))))

(defn c-helper [op]
  (let [n (atom -1)]
    (fn [input]
      (do (swap! n inc)
          (op input @n)))))

(def major (partial bit-criteria >=))

(def minor (partial bit-criteria <))

(def major* (c-helper major))

(def minor* (c-helper minor))

(defn life-support-rating [input]
  (letfn [(helper [cr input]
            (cond (= 1 (count input)) (first input)
                  :else (helper cr (cr input))))]
    (let [ox-rating (comp to-dec (partial helper major*))
          co2-rating (comp to-dec (partial helper minor*))]
      (* (ox-rating input) (co2-rating input)))))

(def td
  (map (fn [s] (map #(Character/getNumericValue %) s))
       '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010")))


(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (binary-diagno read-input)))
  (pp/pprint (format "Problem two: %d" (life-support-rating read-input))))
