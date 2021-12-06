(ns day5
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn parse-line [line]
  (->> (str/split line #"->")
       (map str/trim)
       (map #(str/split % #","))
       (map (fn [s] (map #(Integer/parseInt %) s)))))

(def read-input
  (let [input (get-lines "input/day5")]
    (map parse-line input)))

(defn range* [f from to]
  (let [helper
        (fn [a b]
          (cond
            (> a b) (range a (dec b) -1)
            :else (range a (inc b))))]
    (helper (f from) (f to))))

(defn qualify [from to]
  (or (= (first from) (first to))
      (= (last from) (last to))))

(defn point-in-line [from to]
  (let [x (range* first from to)
        y (range* last from to)
        rp (fn [x y] (cond (= 1 (count x)) (repeat (count y) (first x))
                          :else x))
        xl (rp x y)
        yl (rp y x)]
    (map list xl yl)))

(defn hydro-venture [p]
  (->> p
       (filter #(apply qualify %))
       (mapcat #(apply point-in-line %))
       (frequencies)
       (vals)
       (filter #(> % 1))
       (count)))

(defn hydro-venture2 [p]
  (->> p
       (mapcat #(apply point-in-line %))
       (frequencies)
       (vals)
       (filter #(> % 1))
       (count)))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (hydro-venture read-input)))
  (pp/pprint (format "Problem two: %d" (hydro-venture2 read-input))))
