(ns aoc2020.core
  (:gen-class))

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
(defn find-two-2020
  "finds two integers that sum to 2020 from a list"
  [input]
  (flatten
   (for [n (range (count input))
         :let [x (nth input n)
               xs (nthrest input n)
               result (for [y xs
                            :when (= 2020 (+ x y))]
                        [x y])
               ]
         :when (> (count result) 0)
         ]
     result
     )
   )
  )

(defn calc-two-product
  "Finds two integers which add to 2020 and calculates their product"
  [input]
  (reduce * (find-two-2020 input))
  )

(defn find-three-2020
  "finds three integers that sum to 2020 from a list"
  [input]
  (flatten
   (for [k (range (count input))
         :let [z (nth input k)
               zs (nthrest input k)
               result2 (for [n (range (count zs))
                             :let [x (nth zs n)
                                   xs (nthrest zs n)
                                   result (for [y xs
                                                :when (= 2020 (+ x y z))]
                                            [x y z])
                                   ]
                             :when (> (count result) 0)
                             ]
                         result
                         )
               ]
         :when (> (count result2) 0)
         ]
     result2
     )
   )
  )

(defn calc-three-product
  "Finds three integers which add to 2020 and calculates their product"
  [input]
  (reduce * (find-three-2020 input))
  )

(defn -main
  "Advent of Code 2020."
  [& args]
  (let [input (read-input "resources/input_1.txt")]
    (println "1.1 Given X + Y = 2020 we have X * Y =" (calc-two-product input) )
    (println "1.2 Given X + Y + Z = 2020 we have X * Y * Z =" (calc-three-product input) )
    )
  )
