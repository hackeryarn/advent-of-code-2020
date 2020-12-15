(ns aoc-2020.day5
  (:require [clojure.string :as str]))

(defn binary-text->number [char row]
  (->> row
       reverse
       (map-indexed (fn [i c]
                      (if (= c char)
                        (Math/pow 2 i)
                        0)))
       (reduce +)
       int))


(defn partition->id [partition]
  (let [row-str (take 7 partition)
        column-str (drop 7 partition)
        row (binary-text->number \B row-str)
        column (binary-text->number \R column-str)]
    (+ column (* row 8))))

(defn find-missing-seat [expected seats]
  (if (= expected (first seats))
    (recur (inc expected) (next seats))
    expected))


(->> (slurp "input/day5.txt")
     str/split-lines
     (map partition->id)
     sort
     (#(find-missing-seat (first %) %)))
