(ns aoc-2020.day10
  (:require [clojure.string :as str]))

(def sample
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(defn calc-jolt-difference
  [jolts]
  (let [one-jolt-diffs (filter #(= 1 %) jolts)
        three-jolt-diffs (filter #(= 3%) jolts)]
    (* (count one-jolt-diffs) (+ 1 (count three-jolt-diffs)))))

(def count-combinations
  (memoize
   (fn
     ([x] 1)
     ([x y & zs]
      (if (< 3 (+ x y))
        (apply count-combinations y zs)
        (+
         (apply count-combinations y zs)
         (apply count-combinations (+ x y) zs)))))))
  

(->> (slurp "input/day10.txt")
     str/split-lines
     (map read-string)
     (cons 0)
     sort
     (partition 2 1)
     (map (fn [[x y]] (- y x)))
     (#(apply count-combinations %)))
     
     

