(ns aoc2020.core
  (:gen-class))

(use 'clojure.math.combinatorics)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (clojure.string/split-lines input))) 
  )

(defn read-input-csv
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (re-seq #"[^,\n]+" input))) 
  )

;; day 1
(defn is2020?
  "true if sum is 2020"
  [xs]
  (= 2020 (reduce + xs))
  )

(defn calc-two-product
  "Finds two integers which add to 2020 and calculates their product"
  [input]
  (->> 
   (combinations input 2)
   (map vec)
   (filter is2020?)
   flatten
   (reduce *)
   )
  )

(defn calc-three-product
  "Finds two integers which add to 2020 and calculates their product"
  [input]
  (->> 
   (combinations input 3)
   (map vec)
   (filter is2020?)
   flatten
   (reduce *)
   )
  )


(defn -main
  "Advent of Code 2020."
  [& args]
  (let [input (read-input "resources/input_1.txt")]
    (println "1.1 Given X + Y = 2020 we have X * Y =" (calc-two-product input) )
    (println "1.2 Given X + Y + Z = 2020 we have X * Y * Z =" (calc-three-product input) )
    )
  )
