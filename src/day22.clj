(ns day22
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn parse-line [line]
  (->> (re-seq #"on|off|-?\d+" line)
       ((fn [[op & cord]]
         {:op (keyword op)
          :cube (->> (map #(Integer/parseInt %) cord)
                     (partition 2)
                     (mapv vec))}))))

(def input
  (map parse-line (get-lines "input/day22")))

(defn outside? [a b]
  (let [outside-ax? (fn [x y ax]
                      (let [[x-min x-max] (nth x ax)
                            [y-min y-max] (nth y ax)]
                        (or (> x-min y-max) (> y-min x-max))))]
    (some #(outside-ax? a b %) [0 1 2])))

(defn overlap-ax [a b ax]
  (let [[a-min a-max] (nth a ax)
        [b-min b-max] (nth b ax)]
    (->> [[a-min (dec b-min)] [(inc b-max) a-max]]
         (filter #(apply <= %))
         (into [[(max a-min b-min) (min a-max b-max)]])
         (map #(assoc a ax %)))))

(defn negative [a b]
  (if (outside? a b)
    (list a)
    (reduce (fn [[a & negative] ax]
              (concat (overlap-ax a b ax) negative))
            [a]
            [0 1 2])))

