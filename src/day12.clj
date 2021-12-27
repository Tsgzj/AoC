(ns day12
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.set :as set]) )

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn parse-input [lines]
  (->> lines
       (map #(str/split % #"-"))
       (mapcat (fn [[c1 c2]] [{c1 #{c2}} {c2 #{c1}}]))
       (apply merge-with set/union)))

(def graph
  (parse-input (get-lines "input/day12")))

(defn big-cave? [c]
  (= c (str/upper-case c)))

(def start "start")

(def end "end")

(defn dfs [path limit]
  (let [cur (peek path)
        small-cv (filter (complement big-cave?) path)
        visit-cnt (-> small-cv frequencies (dissoc start) vals)
        visited (if (some #{limit} visit-cnt)
                  (set small-cv)
                  #{start})
        neighbours (remove visited (graph cur))]
    (if (= end cur)
      [path]
      (mapcat #(dfs (conj path %) limit) neighbours))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (count (dfs [start] 1))))
  (pp/pprint (format "Problem two: %d" (count (dfs [start] 2)))))
