(ns aoc-2020.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn unique-answers [line]
  (->> (str/split-lines line)
       (map #(-> % char-array chars set))
       (apply set/intersection)
       count))

(->> (str/split (slurp "input/day6.txt") #"\n\n")
     (map unique-answers)
     (reduce +))
