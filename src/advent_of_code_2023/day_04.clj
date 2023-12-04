(ns advent-of-code-2023.day-04
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.math :as math]))


(def scratch-cards
  (-> (slurp (io/resource "day-04"))
      str/split-lines))

(defn seq-of-seq-of-str-to-seq-of-seq-of-int [seq]
  (map (fn [v] (map #(Integer/parseInt %) v)) seq))

(defn split-winnum-numbers [cards]
  (->> (map #(str/split % #"[:|]") cards)
       ;; (map identity)))
       (map #(map str/trim %))
       (map rest)
       (map (fn [num-pair-giant-str] (map #(str/split % #"\s+") num-pair-giant-str)))
       (map seq-of-seq-of-str-to-seq-of-seq-of-int)
       (map #(hash-map :winnum (first %) :numbers (second %)))))

(defn part-1 [cards]
  (->> (split-winnum-numbers cards)
       (map #(set/intersection (set (:winnum %)) (set (:numbers %))))
       (map count)
       (map #(if (= 0 %)
                 0
                 (math/pow 2 (dec %))))
       (reduce +)))


(first (split-winnum-numbers scratch-cards))
(#(str/split % #"[:|]") (first scratch-cards))
(first (part-1 scratch-cards))
(part-1 scratch-cards)

