(ns aoc-2020.day9
  (:require [clojure.string :as str]))

(def sample "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(def preamble-size 25)

(defn sums
  [nums]
  (set
   (for [x1 nums
         x2 nums]
     (+ x1 x2))))

(defn find-missing-in-sums
  [nums]
  (let [ss (->> nums (partition preamble-size 1) (map sums))]
    (->> (map (fn [n s] (if (some #(= n %) s)
                          0
                          n))
              (drop preamble-size nums) ss)
         (remove #(= 0 %))
         first)))

(defn find-contiguous-sum
  [nums]
  (let [missing (find-missing-in-sums nums)]
    (loop [to-sum 2
           remaining nums]
      (let [sum (->> remaining (take to-sum) (reduce +))]
        (cond
          (= sum missing) (take to-sum remaining)
          (< missing sum) (recur 2 (rest remaining))
          (< (count remaining) to-sum) '()
          :else (recur (inc to-sum) remaining))))))

(defn find-weakness
  [contiguous-sum]
  (+ (apply min contiguous-sum) (apply max contiguous-sum)))
  
(->> (slurp "input/day9.txt")
     str/split-lines
     (map read-string)
     find-contiguous-sum
     find-weakness)
