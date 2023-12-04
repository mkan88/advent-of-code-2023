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
       (map (fn [num-pair-giant-str]
              (map #(str/split % #"\s+") num-pair-giant-str)))
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


(defn get-wins [cards]
  (->> (split-winnum-numbers cards)
       (map #(set/intersection (set (:winnum %)) (set (:numbers %))))
       (map count)))

(defn multiple-update [m ks f]
  (reduce #(update %1 %2 f) m ks))

(defn part-2 [cards]
  (let [wins (get-wins cards)]
    (->> (loop [n-cards (into [] (repeat (count wins) 1))
                wins wins
                total-cards 0]
           (if
             (= 0 (count n-cards))
             total-cards
             (recur
               (into [] (rest
                        (multiple-update n-cards (range 1 (inc (first wins))) #(+ % (first n-cards)))))
               (rest wins)
               (+ total-cards 1 (* (first wins) (first n-cards)))))))))

(get-wins scratch-cards)
(multiple-update [0 1 2 3 4] [1 3] inc)
(part-2 scratch-cards)
;; => 6856328
;; => 6857126
(get-wins scratch-cards)
;; => (8 10 10 6 10 10 10 2 6 4 0 6 2 4 1 0 0 0 5 7 0 2 10 8 8 4 5 2 2 2 1 0 0 10 9 10 10 7 7 3 10 10 5 7 5 4 3 1 1 2 0 0 10 8 10 7 2 10 10 8 4 1 10 0 8 4 1 5 3 4 1 1 1 0 5 10 10 0 10 5 6 10 7 10 6 2 8 8 6 0 0 3 2 1 0 0 8 4 9 8 0 8 10 10 10 10 4 1 5 6 6 5 1 0 1 0 0 6 10 10 10 6 10 6 9 4 8 1 4 8 5 4 3 3 2 2 0 0 10 10 6 10 10 10 9 1 4 10 10 6 1 3 8 4 8 6 4 1 1 2 0 1 0 7 10 2 1 1 4 9 4 7 5 5 4 1 2 2 1 0 8 6 10 10 10 4 10 10 10 1 5 0 8 10 5 0 3 3 4 0 1 1 0 0)
(update [1 3 3] 2 inc)

(range 1 (inc 4))
