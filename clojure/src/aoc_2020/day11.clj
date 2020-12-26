(ns aoc-2020.day11
  (:require [clojure.string :as str]))

(def sample
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(println "----------")

(defn frame->next
  [x [_ loc _ :as y] z]
  (let [filled (count (filter #(= \# %) (concat x y z)))]
    (case loc
      \. \.
      \L (if (= filled 0) \# \L)
      \# (if (<= filled 4) \# \L))))
        

(defn count-rounds-to-stability
  [board]
  (let [empty-line (apply str (take (count (first board)) (repeat ".")))
        next-board (->> board
                        vec
                        (#(conj % empty-line))
                        (cons empty-line)
                        (map #(str "." % "."))
                        (partition 3 1)
                        (map (fn [rows] (map #(partition 3 1 (seq %)) rows)))
                        (map #(apply map frame->next %))
                        (map str/join))]
    (if (= board next-board)
      (count (filter #(= \# %) (apply str board)))
      (recur next-board))))
    

(->> (slurp "input/day11.txt")
     (#(str/replace % #"L" "#"))
     str/split-lines
     count-rounds-to-stability)
     

