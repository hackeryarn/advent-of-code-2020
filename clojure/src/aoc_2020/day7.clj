(ns aoc-2020.day7
  (:require [clojure.string :as str]))

(def test-input "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(defn- contents-str->contents-map
  [contents]
  (->> contents
       (filter #(not= % "no other"))
       (map (fn [s] (str/split s #" " 2)))
       (map (fn [[v k]] [k (Integer. v)]))
       (into {})))

(defn bags-data->bags-map
  [bags]
  (->> bags
       (map #(-> % (str/split #"bags?,? ?(contain)? ?\.?")
                 (->> (map str/trim))))
       (reduce (fn [m bags] (assoc m (first bags)
                                   (contents-str->contents-map
                                    (rest bags))))
               {})))

(defn count-bags-inside
  ([bags]
   (count-bags-inside bags "shiny gold"))
  ([bags bag]
   (let [contents (get bags bag)]
     (+ (reduce + (or (vals contents) [0]))
        (reduce + (map (fn [[b c]] (* c (count-bags-inside bags b)))
                       contents))))))

(->> (slurp "input/day7.txt")
     str/split-lines
     bags-data->bags-map
     count-bags-inside)
