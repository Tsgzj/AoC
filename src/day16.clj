(ns day16
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def file (get-lines "input/day16"))

(def hex
  {\0  "0000" \1  "0001" \2  "0010" \3  "0011"
   \4  "0100" \5  "0101" \6  "0110" \7  "0111"
   \8  "1000" \9  "1001" \A  "1010" \B  "1011"
   \C  "1100" \D  "1101" \E  "1110" \F  "1111"})

(def input
  (mapcat hex (first file)))

(defn bin->num [bits]
  (Long/parseLong (apply str bits) 2))

(defprotocol Packet
  (evalp [p])
  (verp [p]))

(defrecord Literal [ver typ value]
  Packet
  (evalp [p] value)
  (verp [p] ver))

(defrecord Operation [ver typ fun operands]
  Packet
  (evalp [p] (fun (map evalp operands)))
  (verp [p] (reduce + ver (map verp operands))))

(defmulti parse
  (fn [bits] (bin->num (take 3 (drop 3 bits)))))

(defmethod parse 4 [bits]
  (let [ver (bin->num (take 3 bits))
        typ (bin->num (take 3 (drop 3 bits)))]
    (loop [rem (drop 6 bits)
           data []]
      (if (= \0 (first rem))
        [(->Literal ver typ (bin->num (into data (take 4 (rest rem))))) (drop 5 rem)]
        (recur (drop 5 rem)
               (into data (take 4 (rest rem))))))))
