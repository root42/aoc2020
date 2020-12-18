(ns aoc2020.core
  (:gen-class)
  (:require [clojure.set :refer [difference union intersection]])
  (:require [clojure.math.numeric-tower :as math])
  )

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
      [p (first (re-seq #"^(\d+)-(\d+) ([a-z]): ([a-z]+)" str))]
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
         (re-matches #"\d\d\d\d\d\d\d\d\d" pid)
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
         (map #(re-seq #"([NSEWLRF])(\d+)" %))
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

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))
 
(defn chinese-remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                      ; (map vector n a) is same as
        ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line

(defn read-crt-input
  [input]
  (let [lines (slurp input)
        values (->> lines
                    clojure.string/split-lines
                    second
                    (re-seq #"[^,]+"))
        n (->> values
               (filter #(not= "x" %))
               (map #(Integer/parseInt %))
               )
        a (->> values
               (map-indexed
                (fn [idx itm]
                  (if (not= "x" itm)
                    (let [x (Integer/parseInt itm)]
                      (mod (- x (mod idx x)) x)))))
               (filter #(not (nil? %)))
           )
        ]
    [n a]
    )
  )

;; day 14
(defn calc-bit-mask
  [s]
  {:or  (Long/parseLong (clojure.string/replace s #"X" "0") 2) 
   :and (Long/parseLong (clojure.string/replace s #"X" "1") 2)
   :s s}
  )

(defn parse-bit-line
  [line]
  (let [tokens (first (re-seq #"(mask|mem)[\[]?(\d+)?[\]]? = ([0-9X]+)" line))
        opcode (nth tokens 1)
        arg1 (nth tokens 2)
        arg2 (nth tokens 3)]
    (case opcode
      "mask" {:opcode "mask" :mask (calc-bit-mask arg2)}
      "mem" {:opcode "mem" :addr (Integer/parseInt arg1) :val (Long/parseLong arg2) }
      )
    )
  )

(defn read-bit-program
  [input]
  (->> input
       slurp
       clojure.string/split-lines
       (map parse-bit-line)
       )
  )

(defn apply-bit-mask
  [mask val]
  (bit-and (bit-or val (:or mask)) (:and mask))
  )

(defn run-bit-program
  [program]
  (loop [rest program
         mask {:or 0 :and 1}
         mem {}]
    (if (empty? rest)
      mem
      (let [instruction (first rest)
            opcode (:opcode instruction)]
        (case opcode
          "mask" (recur (drop 1 rest) (:mask instruction) mem)
          "mem" (recur (drop 1 rest) mask (assoc mem (:addr instruction) (apply-bit-mask mask (:val instruction))))
          )
        )
      )
    )
  )

(defn prepend-bit
  [i bit addrs]
  (let [f (fn [a] (case bit
                    0 (bit-clear a i)
                    1 (bit-set a i)))]
    (vec (map f addrs))
    )
  )

(defn generate-addrs
  [i addrs]
  (loop [rest addrs
         new []]
    (if (empty? rest)
      new
      (let [cur (first rest)]
        (recur (drop 1 rest) (vec (concat new (prepend-bit i 1 [cur]) (prepend-bit i 0 [cur]))))
        )
      )
    )
  )

(defn bit-get
  [x n]
  (case (bit-test x n)
    false 0
    true 1))

(defn calc-mad-masks
  [mask addr]
  (loop [rest (:s mask)
         addrs [addr]
         i (dec (count rest))]
    (if (empty? rest)
      addrs
      (let [cur (first rest)]
        (case cur
          \X (recur (drop 1 rest) (generate-addrs i addrs) (dec i))
          \0 (recur (drop 1 rest) (prepend-bit i (bit-get addr i) addrs) (dec i))
          \1 (recur (drop 1 rest) (prepend-bit i 1 addrs) (dec i))
          )
        )
      )
    )
  )

(defn apply-mad-mask
  [_mask _mem addr val]
  (let [masks (calc-mad-masks _mask addr)]
    (loop [rest masks
           mem _mem]
      (if (empty? rest)
        mem
        (recur (drop 1 rest) (assoc mem (first rest) val))
        )
      )
    )
  )

(defn run-mad-program
  [program]
  (loop [rest program
         mask {:or 0 :and 1 :s "000000000000000000000000000000000000"}
         mem {}]
    (if (empty? rest)
      mem
      (let [instruction (first rest)
            opcode (:opcode instruction)]
        (case opcode
          "mask" (recur (drop 1 rest) (:mask instruction) mem)
          "mem" (recur (drop 1 rest) mask (apply-mad-mask mask mem (:addr instruction) (:val instruction)))
          )
        )
      )
    )
  )

;; day 15
(defn insert-by-value
  [seq n]
    (loop [idx 2
           rest seq
           mem (transient (vec (repeat n 0)))]
      (if (empty? rest)
        (persistent! mem)
        (recur (inc idx) (drop 1 rest) (assoc! mem (first rest) idx))
        )
      )
  )

(defn memory-game
  [n start]
  (loop [mem-by-value (transient (insert-by-value start n))
         i (inc (count start))
         next (last start)]
    (let [k (nth mem-by-value next)
          age (if (= 0 k) 0 (- i k))]
      (if (= i n)
        age
        (recur (assoc! mem-by-value next i) (inc i) age)
        )
      )
    )
  )

;; day 16
(defn parse-validations
  [input]
  (let [f (fn [line]
            (let [tokens (first (re-seq #"([a-z_]+): (\d+)-(\d+) or (\d+)-(\d+)" line))
                    field (nth tokens 1)
                    min1 (Integer/parseInt (nth tokens 2))
                    max1 (Integer/parseInt (nth tokens 3))
                    min2 (Integer/parseInt (nth tokens 4))
                    max2 (Integer/parseInt (nth tokens 5))]
                [(keyword field) (hash-map :min1 min1 :max1 max1 :min2 min2 :max2 max2)]))]
    (->> input
         (map #(clojure.string/replace-first % #"([a-z]+) ([a-z]+):" "$1_$2:"))
         (filter #(> (count %) 0))
         (map f)
         (into {})
         )
    )
  )

(defn parse-ticket
  [input]
  (->>
   (clojure.string/split input #",")
   (map #(Integer/parseInt %))
   vec
   )
  )

(defn read-ticket-data
  [input]
  (let [lines (slurp input)
        blocks (clojure.string/split lines #"\n\n")
        validations (parse-validations (clojure.string/split-lines (nth blocks 0)))
        my-ticket (parse-ticket (second (clojure.string/split-lines (nth blocks 1))))
        nearby-tickets (map parse-ticket (drop 1 (clojure.string/split-lines (nth blocks 2))))]
    [validations my-ticket nearby-tickets]
    )
  )

(defn test-rules
  [val rules]
  (loop [rest (keys rules)
         valid false]
    (if (or (empty? rest) valid)
      valid
      (let [p (get rules (first rest))
            min1 (get p :min1)
            max1 (get p :max1)
            min2 (get p :min2)
            max2 (get p :max2)
            v (or (and (>= val min1) (<= val max1)) (and (>= val min2) (<= val max2)))]
        (recur (drop 1 rest) v)
        )
      )
    )
  )

(defn validate-ticket
  [ticket rules]
  (filter #(not (test-rules % rules)) ticket)
  )

(defn ticket-scanning-error-rate
  [rules nearby-tickets]
  (->> nearby-tickets 
       (map #(validate-ticket % rules))
       flatten
       (reduce +)
       )
  )

(defn get-valid-tickets
  [rules tickets]
  (let [valid-ticket? (fn [t] (empty? (validate-ticket t rules)))]
    (filter valid-ticket? tickets)
    )
  )

(defn apply-rule
  [rule ticket]
  (let [min1 (get rule :min1)
        max1 (get rule :max1)
        min2 (get rule :min2)
        max2 (get rule :max2)
        f (fn [val] (or (and (>= val min1) (<= val max1)) (and (>= val min2) (<= val max2))))]
    (map f ticket)
    )
  )

(defn combine-result
  [v1 v2]
  (map #(and (first %) (second %)) (map vector v1 v2))
  )

(defn test-rule
  [rule tickets]
  (->> tickets
       (map #(apply-rule rule %))
       (reduce combine-result)
       )
  )

(defn apply-rules-to-tickets
  [rules tickets]
  (loop [field-names (keys rules)
         fields (sorted-map)]
    (if (empty? field-names)
      fields
      (let [field (first field-names)
            result (test-rule (get rules field) tickets)]
        (recur (drop 1 field-names) (assoc fields (count (filter true? result)) [field result]))
        )
      )
    )
  )

(defn update-column
  [column rule-results]
  (loop [rest rule-results
         new-results (sorted-map)]
    (if (empty? rest)
      new-results
      (let [key (first (first rest))
            val (second (first rest))
            field (first val)
            columns (vec (second val))
            new-columns (assoc columns column false)]
        (recur (dissoc rest key) (assoc new-results key [field new-columns]))
        )
      )
    )
  )

(defn determine-field-names
  [tickets rules]
  (let [rule-results (apply-rules-to-tickets rules tickets)]
    (loop [rest rule-results
           field-numbers {}]
      (if (empty? rest)
        field-numbers
        (let [unique (first rest)
              unique-key (first unique)
              cur (second unique)
              cur-key (first cur)
              cur-rule (second cur)
              column (.indexOf cur-rule true)
              new-rest (dissoc rest unique-key)]
          (recur (update-column column new-rest) (assoc field-numbers cur-key column))
          )
        )
      )
    )
  )

(defn get-departure-product
  [rules myticket tickets]
  (let [valid-tickets (get-valid-tickets rules tickets)
        field-names (determine-field-names valid-tickets rules)
        departures [:departure_location :departure_station :departure_platform :departure_track
                    :departure_date :departure_time]]
    (->> departures
         (map field-names)
         (map #(nth myticket %))
         (reduce *)
         )
    )
  )

;; day 17 - taken from https://github.com/elektronaut/advent-of-code/blob/main/day17/day17.clj
(defn surrounding-space
  [[head & tail]]
  (let [heads (map #(+ head %) '(-1 0 1))]
    (if (empty? tail)
      (map list heads)
      (mapcat (fn [t] (map #(cons % t) heads)) (surrounding-space tail))
      )
    )
  )

(defn neighbours
  [cell]
  (->> cell
       surrounding-space
       (filter #(not= cell %))
       set
       )
  )

(defn search-space
  [cells]
  (->> cells
       (map surrounding-space)
       (reduce clojure.set/union)
       set
   )
)

(defn rule
  [cells cell]
  (let [n-count (count (clojure.set/intersection cells (neighbours cell)))]
    (if (contains? cells cell)
      (or (= n-count 2) (= n-count 3))
      (= n-count 3)
      )
    )
  )

(defn next-state
  [cells]
  (->> cells
       search-space
       (filter #(rule cells %))
       set
       )
)

(defn steps [cells]
  (->> cells
       next-state
       steps
       (cons cells)
       lazy-seq
       )
)

(defn parse-input
  [lines]
  (->> (for [y (range 0 (count lines))
             x (range 0 (count (first lines)))]
         (list (nth (nth lines y) x) x y))
       (filter #(= \# (first %)))
       (map rest)
       set
       )
  )

(defn stretch-dimensions
  [N cells]
  (->> cells
       (map #(concat % (repeat (- N 2) 0)))
       set
       )
  )

(defn boot-count
  [input n k]
  (->> input
       parse-input
       (stretch-dimensions n)
       steps
       (drop k)
       first
       count
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
  (let [[n a] (read-crt-input "resources/input_13.txt")]
    (println "13.2 Earliest time when all buses leave staggered: " (chinese-remainder n a))
    )
  (let [program (read-bit-program "resources/input_14.txt")]
    (println "14.1 Sum of all values in memory: " (reduce + (vals (run-bit-program program))))
    (println "14.2 Sum of all values in memory: " (reduce + (vals (run-mad-program program))))
    )
  (let [input '(1,2,16,19,18,0)]
    (println "15.1 2020th spoken number for input: " input (memory-game 2020 input))
    (println "15.2 30000000th spoken number for input: " input (memory-game 30000000 input))
    )
  (let [[rules myticket nearby-tickets] (read-ticket-data "resources/input_16.txt")]
    (println "16.1 Ticket scanning error rate: " (ticket-scanning-error-rate rules nearby-tickets))
    (println "16.2 Product of my ticket's depature fields: " (get-departure-product rules myticket nearby-tickets))
    )
  (let [input (read-text-input "resources/input_17.txt")]
    (println "17.1 Active cell count after 6 cycles in 3D: " (boot-count input 3 6))
    (println "17.2 Active cell count after 6 cycles in 4D: " (boot-count input 4 6))
    )
  )
