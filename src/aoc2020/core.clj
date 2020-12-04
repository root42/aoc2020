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

(defn calc-product
  "Finds N integers which add to 2020 and calculates their product"
  [n input]
  (->>
   (combinations input n)
   (filter is2020?)
   flatten
   (reduce *)
   )
  )

(defn calc-two-product
  "Finds two integers which add to 2020 and calculates their product"
  [input]
  (calc-product 2 input)
  )

(defn calc-three-product
  "Finds three integers which add to 2020 and calculates their product"
  [input]
  (calc-product 3 input)
  )

;; day 2
(defn parse-password-line
  [str]
  (let
      [p (first (re-seq #"^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" str))]
    (vector (Integer. (nth p 1))
            (Integer. (nth p 2))
            (first (nth p 3))
            (nth p 4)
            )
    )
  )

(defn read-password-input
  [input-file]
  (let [input (slurp input-file)]
    (map parse-password-line (clojure.string/split-lines input))
    )
  )

(defn is-valid-password?
  "A valid password P has minimum X and maximum Y occurences of char C"
  [x y c p]
  (let [countc (count (filter #(= c %) p))]
    (and (>= y countc) (<= x countc))
    )
  )

(defn is-toboggan-password?
  "A valid password P has char C either at position X or Y"
  [x y c p]
  (let [a (= c (nth p (dec x)))
        b (= c (nth p (dec y)))]
    (or ; clojure has no xor
     (and (= a true) (= b false))
     (and (= b true) (= a false))
     )
    )
  )

(defn count-valid-passwords
  "Counts the number of valid passwords in the input"
  [input policy]
  (->> input
       (filter #(apply policy %))
       (count)
       )
  )

;; day 3
(defn read-text-input
  [input-file]
  (let [input (slurp input-file)]
    (clojure.string/split-lines input))
  )

(defn count-trees
  "Counts the number of trees the toboggan hits on its way down the given trajectory"
  [input [dx dy]]
  (let [sx (count (first input))
        sy (count input)]
    (loop [x 0
           y 0
           trees 0]
      (if (>= y sy)
        trees
        (let [line (nth input y)
              tree (nth line x)]
          (recur (mod (+ x dx) sx)
                 (+ y dy)
                 (if (= tree \#) (inc trees) trees))
          )
        )
      )
    )
  )

(defn product-count-trees
  "multiplies the number of trees for multiple slopes"
  [input params]
  (reduce * (map #(count-trees input %) params))
  )

;; day 4
(defn read-passport-input
  [input-file]
  (let [input (slurp input-file)
        passports (clojure.string/split input #"\n\n")]
    (->>
     (map #(clojure.string/split % #"[\n: ]") passports)
     (map #(apply hash-map %))
     )
    )
  )

(defn is-valid-passport?
  "is a valid passport if all mandatory fields are contained"
  [passport]
  (let [mandatory ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]]
    (= (count (filter #(contains? passport %) mandatory)) (count mandatory))
    )
  )

(defn count-valid-passports
  "counts the valid passports"
  [input]
  (count (filter is-valid-passport? input))
  )

(defn -main
  "Advent of Code 2020."
  [& args]
  (let [input (read-input "resources/input_1.txt")]
    (println "1.1 Given X + Y = 2020 we have X * Y =" (calc-two-product input) )
    (println "1.2 Given X + Y + Z = 2020 we have X * Y * Z =" (calc-three-product input) )
    )
  (let [input (read-password-input "resources/input_2.txt")]
    (println "2.1 Number of valid passwords: " (count-valid-passwords input is-valid-password?))
    (println "2.2 Number of valid passwords: " (count-valid-passwords input is-toboggan-password?))
    )
  (let [input (read-text-input "resources/input_3.txt")]
    (println "3.1 Toboggan trajectory, number of trees: " (count-trees input [3 1]))
    (println "3.2 Toboggan trajectory, product: " (product-count-trees input '([1 1] [3 1] [5 1] [7 1] [1 2])))
    )
  (let [input (read-passport-input "resources/input_4.txt")]
    (println "3.1 Number of valid passports: " (count-valid-passports input))
    )
  )
