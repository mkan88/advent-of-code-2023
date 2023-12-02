(ns advent-of-code-2023.day-01
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def calibration-doc
  (-> (slurp (io/resource "day-01"))
      str/split-lines))

(defn calibration-values [s]
  (let [digits (re-seq #"\d" s)]
    (->> (list (first digits) (last digits))
         (str/join)
         (#(Integer/parseInt %)))))

(calibration-values (first calibration-doc))

(re-find #"\d" (first calibration-doc))
(re-seq #"\d" (first calibration-doc))
(re-seq #"\d" (second calibration-doc))

(defn part-1 [doc]
  (->> (map calibration-values doc)
       (reduce +)))

(part-1 calibration-doc)

(def numbers
  {:one 1
   :two 2
   :three 3
   :four 4
   :five 5
   :six 6
   :seven 7
   :eight 8
   :nine 9})

(defn replace-text-numbers [s]
  (let [value (numbers (keyword s))]
    (if value value s)))


