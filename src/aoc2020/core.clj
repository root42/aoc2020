(ns aoc2020.core
  (:gen-class)
  (:require [instaparse.core :as insta]))

(use 'clojure.math.combinatorics)
(use 'clojure.set)

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

(defn ranges-valid?
  [passport]
  (let [ecls ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]
        byr (Integer. (passport "byr"))
        iyr (Integer. (passport "iyr"))
        eyr (Integer. (passport "eyr"))
        hgt (passport "hgt")
        hcl (passport "hcl")
        ecl (passport "ecl")
        pid (passport "pid")
        ]
    (and (and (<= 1920 byr) (>= 2002 byr))
         (and (<= 2010 iyr) (>= 2020 iyr))
         (and (<= 2020 eyr) (>= 2030 eyr))
         (re-matches #"#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]" hcl)
         (let [hgts (re-matches #"(.*)(cm|in)" hgt)]
           (if hgts
             (let [height (Integer. (nth hgts 1))
                   unit (last hgts)]
               (case unit
                 "cm" (and (<= 150 height) (>= 193 height))
                 "in" (and (<= 59 height) (>= 76 height))
                 )
               )
             )
           )
         (some #{ecl} ecls)
         (re-matches #"[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]" pid)
         )
    )
  )

(defn is-valid-passport-ranges?
  "is a valid passport if all mandatory fields are contained and "
  [passport]
  (and (is-valid-passport? passport)
       (ranges-valid? passport))
  )

(defn count-valid-passports
  "counts the valid passports"
  [input policy]
  (count (filter policy input))
  )

;; day 5
(defn bisect
  "bisects the range [start,end) using hi and lo as symbols and data being a string of hi and lo symbols"
  [start end hi lo data]
  (loop [d data
         s start
         e end]
    (if (= (count d) 0)
      s ; if d is empty, return result
      (if (= (first d) lo)
        (recur (drop 1 d) s (+ s (/ (- e s) 2))) ; lower half
        (recur (drop 1 d) (+ s (/ (- e s) 2)) e) ; upper half
        )
      )
    )
  )

(defn calculate-seat-coordinate
  "calculates the seat coordinate according to the bisection of the pass string"
  [pass]
  (let [bp (split-at 7 pass)
        row (first bp)
        col (second bp)]
    {:row (bisect 0 128 \B \F row), :col (bisect 0 8 \R \L col)}
    )
  )

(defn calculate-seat-id
  "for a given boarding pass calculates the seat ID"
  [pass]
  (let [coord (calculate-seat-coordinate pass)]
    (+ (* (:row coord) 8) (:col coord))
    )
  )

(defn highest-seat-id
  "returns the highest seat ID encountered in given input"
  [input]
  (->> input
       (map calculate-seat-id)
       (sort)
       (last)
   )
  )

(defn find-gap
  "finds a gap in a linear, sorted sequence of integers"
  [input]
  (loop [i input]
    (let [a (nth i 0)
          b (nth i 1)]
      (if (= (inc a) b)
        (recur (drop 1 i)) ;; else continue search
        (inc a) ;; found our seat as a+1
        )
      )
    )
  )

(defn my-seat-id
  "returns my seat ID, which is the only one missing, except for first and last row"
  [input]
  (->> input
       (map calculate-seat-id)
       (sort)
       (find-gap)
       )
  )

;; day 6
(defn read-text-block-input
  "reads in blocks of text that is separated by two newlines"
  [input-file]
  (let [input (slurp input-file)]
    (clojure.string/split input #"\n\n")
    )
  )

(defn sum-of-anyone-answers
  "calculates the sum of all answers where anyone of each group answered"
  [input]
  (->> input
       (map #(clojure.string/replace % #"\n" "" ))
       (map set)
       (map count)
       (reduce +)
       )
  )

(defn sum-of-everyone-answers
  "calculates the sum of all answers where everyone of each group answered"
  [input]
  (->> input
       (map clojure.string/split-lines)
       (map #(map set %))
       (map #(apply clojure.set/intersection %))
       (map count)
       (reduce +)
       )
  )

;; day 7
(defn rule-string
  "creates an EBNF grammar string from the input"
  [input]
  (-> input
      (clojure.string/replace #"([a-z]+) ([a-z]+) bags contain no other bags." "$1_$2=\"$1_$2\"")
      (clojure.string/replace #"bag[s]?[ .,]" "")
      (clojure.string/replace #"([a-z]+) ([a-z]+) contain " "$1_$2=\"$1_$2\"|")
      (clojure.string/replace #"([a-z]+) ([a-z]+)" "$1_$2")
      (clojure.string/replace #"[0-9]+" "")
      (clojure.string/replace #"  " "|")
      )
  )

(defn get-start-symbols
  [rules]
  (->> rules
       clojure.string/split-lines
       (map #(clojure.string/split % #"="))
       (map #(take 1 %))
       flatten
       (map keyword)
       )
  )

(defn bags-that-contain
  "returns the number of different bags that can containe a given bag"
  [mybag input]
  (let [rules (rule-string input)
        parser (insta/parser rules)
        start-symbols (get-start-symbols rules)]
    (->> start-symbols
         (map #(parser mybag :start %))
         (filter #(not (insta/failure? %)))
         flatten
         (filter #(and (not= % (keyword mybag)) (not= % mybag)))
         set
         count
         )
    )
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
    (println "4.1 Number of valid passports: " (count-valid-passports input is-valid-passport?))
    (println "4.2 Number of valid passports: " (count-valid-passports input is-valid-passport-ranges?))
    )
  (let [input (read-text-input "resources/input_5.txt")]
    (println "5.1 Highest seat ID: " (highest-seat-id input))
    (println "5.2 My seat ID: " (my-seat-id input))
    )
  (let [input (read-text-block-input "resources/input_6.txt")]
    (println "6.1 Sum of groups where anyone answers: " (sum-of-anyone-answers input))
    (println "6.2 Sum of groups where everyone answers: " (sum-of-everyone-answers input))
    )
  (let [input (slurp "resources/input_7.txt")]
    (println "7.1 Bags that can contain shiny gold bag: " (bags-that-contain "shiny_gold" input))
    )
  )
