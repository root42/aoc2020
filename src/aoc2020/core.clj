(ns aoc2020.core
  (:gen-class)
  (:require [clojure.set :refer [difference union intersection]]))

(use 'clojure.math.combinatorics)
(use 'clojure.set)

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (map #(read-string %) (clojure.string/split-lines input))))
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
   first
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
(defn parse-bag-contents
  [line]
  (let [[_ color contained] (re-matches #"([a-z\ ]+) bags contain (.+)" line)]
    [color
     (->> contained
          (re-seq #"(\d+) ([a-z\ ]+) bag")
          (map (fn [[_ n color]] [color (Integer/parseInt n)]))
          (into {})
          )]
    )
  )

(defn create-rule-map
  "creates a map of the rules"
  [input]
  (->> input
       clojure.string/split-lines
       (map parse-bag-contents)
       (into {} )
       )
  )

(defn bag-contains
  [[bag contents] query rules]
  (if (contains? contents query)
    true
    (some #(bag-contains [(first %) (get rules (first %))] query rules) contents)
    )
  )

(defn count-bags-that-contain
  "counts the number of bags that contain directly or indirectly a given bag"
  [input bag]
  (let [rules (create-rule-map input)]
    (->> rules
         (filter #(bag-contains % bag rules))
         count
         )
    )
  )

(defn count-bag-contents
  "recursive part of couting, uses 1 as the accumulator, otherwise we forget to count the current bag"
  [rules bag a]
  (let [b (get rules bag 1)]
    (reduce
     (fn [acc [color n]]
       (+ acc (* n (count-bag-contents rules color 1)))
       )
     a
     b)
    )
  )

(defn count-bag
  "kicks off the actual counting, initializing the accumulator to 0"
  [rules bag]
  (count-bag-contents rules bag 0)
  )

(defn contents-of-bag
  "counts the contents of a given bag"
  [input bag]
  (let [rules (create-rule-map input)]
    (count-bag rules bag)
    )
  )

;; day 8
(defn does-halt?
  "Returns a tuple [b acc] where b is true or fals depending on if the program halts and acc is the value of the
  accumulator."
  [program]
  (loop [pc 0
         acc 0
         seen #{}]
    (if (>= pc (count program))
      [true acc]
      (if (contains? seen pc)
        [false acc]
        (let [instruction (nth program pc)
              opcode (first instruction)
              arg (second instruction)
              ]
          (case opcode
            "nop" (recur (inc pc) acc (conj seen pc))
            "acc" (recur (inc pc) (+ acc arg) (conj seen pc))
            "jmp" (recur (+ pc arg) acc (conj seen pc))
            )
          )
        )
      )
    )
  )

(defn decode-program
  "Decodes a given string into a list of program instructions [opcode arg]."
  [input]
  (let [lines (clojure.string/split-lines input)
        instructions (map #(clojure.string/split % #" ") lines)
        program (map #(vector (first %) (Integer/parseInt (second %))) instructions)]
    program
    )
  )

(defn fix-program
  "Repairs a program by changing exactly one jmp to nop or the other way around."
  [program]
  (loop [pc 0]
    (let [instruction (nth program pc)
          opcode (first instruction)
          arg (second instruction)
          halted (case opcode
                   "nop" (does-halt? (assoc program pc ["jmp" arg]))
                   "jmp" (does-halt? (assoc program pc ["nop" arg]))
                   "acc" [false 0]
                   )
          ]
      (if (first halted)
        (second halted)
        (recur (inc pc))
        )
      )
    )
  )

(defn detect-infinite-loop
  "Will detect an infinite loop and return the value of acc before the loop."
  [input]
  (->> input
       decode-program
       does-halt?
       second
       )
  )

(defn test-program
  "Tests a program and fixes it so that it will halt, if it can be fixed by replacing exactly one opcode."
  [input]
  (->> input
       decode-program
       vec
       fix-program
       )
  )

;; day 9

;; This is pretty inefficient. A better solution would be to remove and add only thos pairs that are obsolete
;; or new according to the rolling prefix.
(defn is-sum-of-pair?
  "Returns an empty collection if value isn't a sum of any pair of numbers in coll."
  [value coll]
  (filter #(= (reduce + %) value) (combinations coll 2))
  )

(defn find-first-not-sum
  "Finds the first number in the input that is NOT the sum of a rolling prefix of size n"
  [n input]
  (loop [preamble (take n input)
         rest (drop n input)]
    (let [current (first rest)]
      (if (empty? (is-sum-of-pair? current preamble))
        current
        (recur (concat (drop 1 preamble) (vector current)) (drop 1 rest))
        ))))

(defn test-range
  [q i input]
  (loop [n 1
         slice (take 1 (drop i input))
         sum (reduce + slice)] ; inner loop: build longer and longer sequence
    (if (< (+ i n) (count input))
      (if (= sum q)
        (let [sorted (sort slice)
              min (first sorted)
              max (last sorted)]
          (+ min max)
          )
        (if (< sum q)
          (let [k (nth input (+ i n))]
            (recur (inc n) (concat slice (vector k)) (+ sum k)) ; did not find but might, so recur
            )
          nil ; did not find and not possible anymore, continue in outer loop
          )))))

(defn find-sum-of-number
  [q input]
  (loop [i 0] ; loop over whole input
    (if (< i (count input))
      (let [r (test-range q i input)]
        (if (= nil r)
          (recur (inc i)) ; not found yet, recur
          r ; found
          )))))

;; day 10
(defn differences
  [input]
  (loop [v (take 2 input)
         coll input
         diffs []]
    (if (= (count coll) 1)
      diffs
      (recur (take 2 (drop 1 coll)) (drop 1 coll) (conj diffs (- (second v) (first v)))))
    )
  )

(defn product-of-jolt-differences
  [i j input]
  (let [result (->> input
                    sort
                    (into [0])
                    differences
                    frequencies)]
    (* (get result i) (inc (get result j)))
    )
  )

;; day 10.2
(defn create-adapter-dag
  "Creates the DAG for the given input of adapters with max of 3 Jolts between adapters"
  [input]
  (let [sorted (->> input
                    sort
                    (into [0]))
        builtin (+ 3 (reduce max sorted))
        all-adapters (conj sorted builtin)]
    (loop [dag {}
           coll all-adapters
           current-adapter (first all-adapters)]
      (if (empty? coll)
        dag
        (let [edges (->> coll
                         (map #{(+ 1 current-adapter) (+ 2 current-adapter) (+ 3 current-adapter)})
                         (filter #(not= nil %))
                         set ; probably not necessary on this input, but makes it more safe
                         vec
                         sort)]
          (recur (into dag {current-adapter edges}) (drop 1 coll) (first coll))
          )
        )
      )
    )
  )

(defn number-of-paths
  "Calculates number of possible paths in a DAG with Dynamic Programming"
  [input]
  (let [dag (create-adapter-dag input)
        s (sort (keys dag))
        destination (last s)]
    (loop [dp {destination 1}
           i (dec (count s))]
      (if (>= i 0)
        (let [si (nth s i)
              dpsi (loop [vsi (dag si)
                          dpsi (if (nil? (dp si)) 0 (dp si))]
                     (if (empty? vsi)
                       dpsi
                       (let [vsij (first vsi)
                             dpvsij (if (nil? (dp vsij)) 0 (dp vsij))]
                         (recur (drop 1 vsi) (+ dpsi dpvsij)))
                       )
                     )]
          (recur (assoc dp si dpsi) (dec i))
          )
        (dp 0) ; This is our result
        )
      )
    )
  )

;; day 11
(defn get-cell
  [i j state]
  (let [row (get state i)]
    (get row j))
  )

(defn get-direct-neighbors
  [[i j] state]
  (let [nbs [(get-cell (dec i) (dec j) state)
             (get-cell (dec i)      j  state)
             (get-cell (dec i) (inc j) state)
             (get-cell      i  (dec j) state)
             (get-cell      i  (inc j) state)
             (get-cell (inc i) (dec j) state)
             (get-cell (inc i)      j  state)
             (get-cell (inc i) (inc j) state)
             ]]
    (->> nbs
         (filter #(= % \#))
         count
         )
    )
  )

(defn ray-cast
  [[ox oy] [dx dy] state]
  (loop [[px py] [(+ ox dx) (+ oy dy)]]
    (let [seat (get-cell px py state)]
      (if (not= seat \.)
        seat
        (recur [(+ px dx) (+ py dy)])
        )
      )    
    )
  )

(defn get-line-of-sight-neighbors
  [origin state]
  (let [nbs [(ray-cast origin [-1  1] state)
             (ray-cast origin [ 0  1] state)
             (ray-cast origin [ 1  1] state)
             (ray-cast origin [-1  0] state)
             (ray-cast origin [ 1  0] state)
             (ray-cast origin [-1 -1] state)
             (ray-cast origin [ 0 -1] state)
             (ray-cast origin [ 1 -1] state)
             ]]
    (->> nbs
         (filter #(= % \#))
         count
         )
    )
  )

(defn update-state
 [state get-neighbors max-neighbors]
 (loop [i 0
        changes 0
        newstate []]
   (if (>= i (count state))
     [changes newstate]
     (let [row (nth state i)
           [chg col] (loop [j 0
                            chgcol 0
                            newcolumn []]
                       (if (< j (count row))
                         (let [neighbors (get-neighbors [i j] state)
                               oldcell (get-cell i j state)
                               cell (cond
                                      (and (= oldcell \L) (= neighbors 0)) \#
                                      (and (= oldcell \#) (>= neighbors max-neighbors)) \L
                                      :else oldcell
                                      )]
                           (recur (inc j)
                                  (if (not= cell oldcell)
                                    (inc chgcol)
                                    chgcol)
                                  (conj newcolumn cell))
                           )
                         [chgcol newcolumn]
                         )
                       )
           ]
       (recur (inc i) (+ changes chg) (vec (concat newstate (vector col))))
       )
     )
   )
 )

(defn seats-occupied
  [input get-neighbors max-neighbors]
  (println)
  (loop [state input]
    (let [[changes newstate] (update-state state get-neighbors max-neighbors)]
      (print ".") (flush)
      (if (= 0 changes)
        (->> newstate
             (apply str)
             (filter #(= \# %))
             count
             )
        (recur newstate)
        )
      )
    )
  )

;; day 12
(defn abs
  [n]
  (max n (-' n)))

(defn manhattan
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn read-nav-input
  [input]
  (let [lines (slurp input)]
    (->> lines
         clojure.string/split-lines
         (map #(re-seq #"([NSEWLRF])([0-9]+)" %))
         (map #(first %))
         (map #(drop 1 %))
         (map #(vector (first %) (Integer/parseInt (second %))))
         )
    )
  )

(defn rotate
  [[dx dy] steps]
  (case (mod steps 360)
    0   [dx dy]
    90  [(-' dy) dx]
    180 [(-' dx) (-' dy)]
    270 [dy (-' dx)]
    )
  )

(defn rotate-left
  [dir angle]
  (rotate dir angle)
  )

(defn rotate-right
  [dir angle]
  (rotate dir (- 360 angle))
  )

(defn navigate-ship
  [input]
  (loop [navdata input
         pos [0 0]
         dir [1 0]
         ]
    (if (empty? navdata)
      pos
      (let [nav (first navdata)
            action (first nav)
            distance (second nav)]
        (case action
          "N" (recur (drop 1 navdata) [(first pos)                 (+ (second pos) distance)    ] dir)
          "E" (recur (drop 1 navdata) [(+ (first pos) distance)    (second pos)                 ] dir)
          "S" (recur (drop 1 navdata) [(first pos)                 (- (second pos) distance)    ] dir)
          "W" (recur (drop 1 navdata) [(- (first pos) distance)    (second pos)                 ] dir)
          "F" (recur (drop 1 navdata)
                     [(+ (first pos) (* distance (first dir)))
                      (+ (second pos) (* distance (second dir)))] dir)
          "L" (recur (drop 1 navdata) pos (rotate-left dir distance))
          "R" (recur (drop 1 navdata) pos (rotate-right dir distance))
          )
        )
      )
    )
  )

(defn navigate-ship-waypoint
  [input]
  (loop [navdata input
         pos [0 0]
         wp [10 1]
         ]
    (if (empty? navdata)
      pos
      (let [nav (first navdata)
            action (first nav)
            distance (second nav)]
        (case action
          "N" (recur (drop 1 navdata) pos [(first wp)              (+ (second wp) distance)    ])
          "E" (recur (drop 1 navdata) pos [(+ (first wp) distance) (second wp)                 ])
          "S" (recur (drop 1 navdata) pos [(first wp)              (- (second wp) distance)    ])
          "W" (recur (drop 1 navdata) pos [(- (first wp) distance) (second wp)                 ])
          "F" (recur (drop 1 navdata)
                     [(+ (first pos) (* distance (first wp)))
                      (+ (second pos) (* distance (second wp)))]
                     wp)
          "L" (recur (drop 1 navdata) pos (rotate-left wp distance))
          "R" (recur (drop 1 navdata) pos (rotate-right wp distance))
          )
        )
      )
    )
  )

;; day 13
(defn index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

(defn read-bus-input
  [input]
  (let [lines (clojure.string/split-lines (slurp input))
        earliest (Integer/parseInt (first lines))
        buses (->> lines
                   second
                   (re-seq #"[^,]+")
                   (filter #(not= "x" %))
                   (map #(Integer/parseInt %))
                   )
        ]
    [earliest buses]
    )
  )

(defn get-bus-wait
  [t l]
  (if (= 0 (rem t l))
    0
    (- (* (inc (quot t l)) l) t)
    )
  )

(defn earliest-bus
  [[earliest buses]]
  (let [waits (map #(get-bus-wait earliest %) buses)
        wait (reduce min waits)
        bus (nth buses (index-of wait waits))
        ]
    (* wait bus)
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
    (println "7.1 Bags that can contain shiny gold bag: " (count-bags-that-contain input "shiny gold"))
    (println "7.2 Contents of shiny gold bag: " (contents-of-bag input "shiny gold"))
    )
  (let [input (slurp "resources/input_8.txt")]
    (println "8.1 Value of accumulator before infinite loop: " (detect-infinite-loop input))
    (println "8.2 Value of accumulator after fixing program: " (test-program input))
    )
  (let [input (read-input "resources/input_9.txt")
        n (find-first-not-sum 25 input)]
    (println "9.1 First value that is not a sum of its 25 predecessors: " n)
    (println "9.2 Encryption weakness: " (find-sum-of-number n input))
    )
  (let [input (read-input "resources/input_10.txt")]
    (println "10.1 Product of number of joltage differences (1,3): " (product-of-jolt-differences 1 3 input))
    (println "10.2 Number of ways to connect the adapters: " (number-of-paths input))
    )
  (let [input (read-text-input "resources/input_11.txt")]
    (println "11.1 Seats occupied: " (seats-occupied input get-direct-neighbors 4))
    (println "11.2 Seats occupied: " (seats-occupied input get-line-of-sight-neighbors 5))
    )
  (let [input (read-nav-input "resources/input_12.txt")]
    (println "12.1 Manhattan distance of navigation: " (manhattan [0 0] (navigate-ship input)))
    (println "12.1 Manhattan distance of navigation with waypoint: " (manhattan [0 0] (navigate-ship-waypoint input)))
    )
  (let [input (read-bus-input "resources/input_13.txt")]
    (println "13.1 Earliest bus times waiting time in minutes: " (earliest-bus input))
    )
  )
