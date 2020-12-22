(ns aoc-2020.day8
  (:require [clojure.string :as str]))

(def sample "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn run-nop
  [{:keys [loc] :as state}]
  (-> state
      (update :loc inc)
      (update :visited #(conj % loc))))

(defn run-acc
  [{:keys [loc] :as state} by]
  (-> state
      (update :loc inc)
      (update :acc #(+ % by))
      (update :visited #(conj % loc))))

(defn run-jmp
  [{:keys [loc] :as state} by]
  (-> state
      (update :loc #(+ % by))
      (update :visited #(conj % loc))))

(defn test-path
  [locs {:keys [loc acc visited] :as state}]
  (cond
    (contains? visited loc) nil
    (<= (count locs) loc) acc
    :else (let [[instruction by-str] (nth locs loc)
                by (Integer/parseInt by-str)]
            (case instruction
              "nop" (recur locs (run-nop state))
              "acc" (recur locs (run-acc state by))
              "jmp" (recur locs (run-jmp state by))))))

(declare run-prog)

(defn run-try
  [locs [f1 f2]]
  (or (run-prog locs (f1)) (test-path locs (f2))))

(defn run-prog
  [locs {:keys [loc acc visited] :as state}]
  (cond
    (contains? visited loc) nil
    (<= (count locs) loc) acc
    :else (let [[instruction by-str] (nth locs loc)
                by (Integer/parseInt by-str)
                nopf #(run-nop state)
                jmpf #(run-jmp state by)]
            (case instruction
              "nop" (run-try locs [nopf jmpf])
              "acc" (recur locs (run-acc state by))
              "jmp" (run-try locs [jmpf nopf])))))

(->> (slurp "input/day8.txt")
     str/split-lines
     (map #(str/split % #" \+?"))
     (#(time (run-prog % {:loc 0 :acc 0 :visited #{}}))))
