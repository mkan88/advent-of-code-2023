(ns advent-of-code-2023.day-03
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))


(def engine-parts
  (-> (slurp (io/resource "day-03"))
      str/split-lines))


(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (. m start) :end (. m end) :group (. m group)}
               (lazy-seq (step))))))))

(defn get-numbers [line]
  (->> (re-seq-pos #"[1234567890]+" line)))

(defn get-symbols [line]
  (->> (re-seq-pos #"[\*+=%\-#@/&$]" line)
       (map :start)))

(defn adjacent-single-number-symbol-pair? [number sym]
  (<= (dec (:start number)) sym (identity (:end number))))

(defn adjacent-number? [number symbols]
  (some true? (map #(adjacent-single-number-symbol-pair? number %) symbols)))

(defn adjacent-line? [[line-one line-two line-three]]
  (let [symbols-one (get-symbols line-one)
        numbers-two (get-numbers line-two)
        symbols-two (get-symbols line-two)
        symbols-three (get-symbols line-three)
        symbols (concat symbols-one symbols-two symbols-three)]
    (filter #(adjacent-number? % symbols) numbers-two)))

(defn part-1 [parts]
  (->> (concat [""] engine-parts [""])
       (partition 3 1)
       (map adjacent-line?)
       (flatten)
       (map :group)
       (map #(Integer/parseInt %))
       (reduce +)))

(first engine-parts)
;; => "...........441.................367................296........................................567..47.....45.................947............."
(second engine-parts)
;; => "...606..........888.....................508..........*892................+..=138.381..967...............*....%......926...........218......."
(nth engine-parts 2)
;; => "....*......116..*..............747............-....................777..460..........*.......549......127...595.......*..290........*.968..."
(re-seq-pos #"[1234567890]+" (second engine-parts))
(get-numbers (second engine-parts))
(distinct (apply str engine-parts))
;; => (\. \4 \1 \3 \6 \7 \2 \9 \5 \0 \8 \* \+ \= \% \- \# \@ \/ \& \$)
(re-seq-pos #"[*+=%\-#@/&$]"(second engine-parts))
(get-symbols (nth engine-parts 2))

(adjacent-single-number-symbol-pair? {:start 81 :end 84 :group 381} 85)
(adjacent-line? [(first engine-parts) (second engine-parts) (nth engine-parts 2)])

(count (concat [""] engine-parts '("")))

(part-1 engine-parts)
