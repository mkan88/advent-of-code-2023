(ns advent-of-code-2023.day-05
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.math :as math]))


(def seeds-maps
  (let [sections (-> (slurp (io/resource "day-05"))
                     (str/split #"\n\n"))
        seeds (as-> (first sections) col
                (second (str/split col #":"))
                (str/trim col)
                (str/split col #"\s+")
                (map #(Long/parseLong %) col))
        line-to-seq-int (fn [s] (->> (map #(str/split % #"\s+") s)
                                     (map (fn [line] (map #(Long/parseLong %) line)))))
        maps (->> (rest sections)
                  (map #(second (str/split % #":\n")))
                  (map str/trim)
                  (map str/split-lines)
                  (map line-to-seq-int))
               ;; )
        ]
    [seeds maps]))

(def seeds (first seeds-maps))
(def maps (second seeds-maps))

(defn seed-remap-line [seed [destination source rang]]
  (if (<= source seed (+ source rang))
    (+ (- seed source) destination)
    seed))

(defn seed-remap [seed lines]
  (loop [l lines]
    (let [mapped (seed-remap-line seed (first l))]
      (cond
        (not= seed mapped) mapped
        (= (count l) 1) seed
        :else (recur (rest l))))))

(defn part-1 [seeds maps]
  (->> (map (fn [s]
              (loop [s s
                     maps maps]
                (if (= 0 (count maps))
                  s
                  (recur (seed-remap s (first maps)) (rest maps))))) seeds)
       (apply min)))

(seed-remap 104847962 (first maps))
(seed-remap-line 104847962 (first (first maps)))
(part-1 seeds maps)

(identity seeds)

(def seeds2
  (->> (partition 2 seeds)
       (map (fn [[start rang]] (range start (+ start rang))))
       (flatten)))

(defn seed-remap-line-range [[seed-min seed-max] [destination source rang]]
  (let [source-max (+ source rang)
        remap-fn (fn [sd] (+ (- sd source) destination))]
  (cond
    (< seed-max source) '(seed-min seed-max)
    (< source-max seed-min) '(seed-min seed-max)
    (<= source seed-min seed-max source-max) '((remap-fn seed-min) (remap-fn seed-max))
    (<= source seed-min source-max seed-max) '('((remap-fn seed-min) (remap-fn source-max))
                                               '(source-max seed-min))
    (<= seed-min source seed-max source-max) '('(seed-min (dec source)) '((remap-fn source) (remap-fn seed-max))))))

(defn seed-remap-range [seed lines]
  (loop [l lines]
    (let [mapped (seed-remap-line-range seed (first l))]
      (cond
        (not= seed mapped) mapped
        (= (count l) 1) seed
        :else (recur (rest l))))))

(defn part-2 [seeds maps]
  (->> (pmap (fn [s]
              (loop [s s
                     maps maps]
                (if (= 0 (count maps))
                  s
                  (recur (seed-remap s (first maps)) (rest maps))))) seeds)
       (apply min)))

(identity seeds2)

(count seeds2)

(range 2 10)
(apply range '(2 10))

(part-2 seeds2 maps)
