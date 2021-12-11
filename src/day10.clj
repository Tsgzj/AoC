(ns day10
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn pairs? [r l]
  (let [parens {\) \( \} \{ \] \[ \> \<}]
    (= (parens r) l)))

(defn left-parens? [c]
  (let [left-parens #{\(  \{  \[ \<}]
    (some? (left-parens c))))

(defn parse-line [l]
  (letfn [(helper [src stk]
            (if (= 0 (count src))
              stk
              (if (left-parens? (peek src))
                (helper (pop src) (conj stk (peek src)))
                (if (pairs? (peek src) (peek stk))
                  (helper (pop src) (pop stk))
                  (peek src))) ))]
    (helper (vec (str/reverse l)) '())))

(def score
  {\) 3 \] 57 \} 1197 \> 25137})

(def syntax-score
  (->> (get-lines "input/day10")
       (map parse-line)
       (filter some?)
       (filter #(not (coll? %)))
       (map score)
       (reduce +)))

(def score-left
  {\( 1 \{ 3 \[ 2 \< 4})

(defn middle [l]
  (let [cnt (count l)
        hlf (int (Math/floor (/ cnt 2)))]
    (->> (sort l)
         (drop hlf)
         first)))

(defn score-auto-complete [l]
  (letfn [(helper [l cur]
            (if (empty? l)
              cur
              (helper (rest l)
                      (->> (first l)
                           score-left
                           (+ (* 5 cur))))))]
    (helper l 0)))

(def auto-complete-score
  (->> (get-lines "input/day10")
       (map parse-line)
       (filter some?)
       (filter coll?)
       (map score-auto-complete)
       middle))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" syntax-score))
  (pp/pprint (format "Problem two: %d" auto-complete-score)))
