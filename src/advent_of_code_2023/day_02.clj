(ns advent-of-code-2023.day-02
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def cube-games
  (-> (slurp (io/resource "day-02"))
      str/split-lines))

(defn cubes-by-game [game]
  (as-> (last (str/split game #":")) col
      (str/split col #";")
      (map cubes-by-subgame col)
      (reduce sum-subgame {"red" 0 "green" 0 "blue" 0} col)))

(defn cubes-in-all-subgames [game]
  (as-> (last (str/split game #":")) col
      (str/split col #";")
      (map cubes-by-subgame col)))

(defn cubes-by-subgame [subgame]
  (->> (str/split subgame #",")
       (map str/trim)
       (map #(str/split % #" "))
       (map #(vector (last %) (Integer/parseInt (first %))))
       (into {})))

(defn sum-subgame [summed to-sum]
  (let [add-one-colour (fn [summed one-colour] (update-in summed [(first one-colour)] #(+ % (last one-colour))))]
    (reduce add-one-colour summed to-sum)))

(defn filter-colour [condition game]
  (let [colours (keys game)]
    (->> (map #(>= (condition %) (game %)) colours)
        (every? true?))))

(defn part-1 [condition games]
  (as-> (map cubes-in-all-subgames games) col
    (map (fn [game] (every? true? (map #(filter-colour condition %) game))) col)
    (map-indexed vector col)
    (map #(list (inc (first %)) (last %)) col)
    (filter #(true? (last %)) col)
    (reduce (fn [x y] (+ x (first y))) 0 col)))

(part-1 {"red" 12 "green" 13 "blue" 14} cube-games)

(defn max-subgame [maxxed to-max]
  (let [max-one-colour (fn [maxxed one-colour] (update-in maxxed [(first one-colour)] #(max % (last one-colour))))]
    (reduce (fn [m t] (reduce max-one-colour m t)) maxxed to-max)))

(defn part-2 [games]
  (as-> (map cubes-in-all-subgames games) col
    (map #(max-subgame {"red" 0 "green" 0 "blue" 0} %) col)
    (map vals col)
    (map #(reduce * 1 %) col)
    (reduce + col)))

(part-2 cube-games)
