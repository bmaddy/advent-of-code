(ns aoc-2019
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is run-tests testing]]
            [clojure.set :as set]
            [clojure.tools.trace :refer [deftrace]]))

(defn fuel
  [mass]
  (- (quot mass 3) 2))

(defn day-1-1
  [input]
  (->> (str "(" input ")")
       edn/read-string
       (map fuel)
       (reduce +)))

(deftest day-1-1-test
  (is (= 2 (day-1-1 "12")))
  (is (= 2 (day-1-1 "14")))
  (is (= 654 (day-1-1 "1969")))
  (is (= 33583 (day-1-1 "100756")))
  (is (= 3252208 (day-1-1 (slurp "day-1.txt")))))

(defn day-1-2
  [input]
  (->> (str "(" input ")")
       edn/read-string
       (mapcat (fn [module-mass]
              (->> module-mass
                   (iterate fuel)
                   rest
                   (take-while pos?))))
       (reduce +)))

(deftest day-1-2-test
  (is (= 2 (day-1-2 "14")))
  (is (= 966 (day-1-2 "1969")))
  (is (= 50346 (day-1-2 "100756")))
  (is (= 4875451 (day-1-2 (slurp "day-1.txt")))))

(defn run-op
  [data f k1 k2 out]
  (let [a (get data k1)
        b (get data k2)]
    (assoc data out (f a b))))

(defn read-syms
  [s]
  (edn/read-string (str "[" s "]")))

(defn day-2
  [input]
  (loop [pos 0
         data input]
    (let [[opcode k1 k2 out] (drop pos data)
          op (get {1 + 2 *} opcode)
          next-pos (+ pos 4)]
      (if (= 99 opcode)
        data
        (recur next-pos (run-op data op k1 k2 out))))))

(deftest day-2-test
  (is (= [2,0,0,0,99] (day-2 (read-syms "1,0,0,0,99"))))
  (is (= [2,3,0,6,99] (day-2 (read-syms "2,3,0,3,99"))))
  (is (= [2,4,4,5,99,9801] (day-2 (read-syms "2,4,4,5,99,0"))))
  (is (= [30,1,1,4,2,5,6,0,99] (day-2 (read-syms "1,1,1,4,99,5,6,0,99"))))
  (is (= 3085697 (-> (slurp "day-2.txt")
                     read-syms
                     (assoc 1 12)
                     (assoc 2 2)
                     day-2
                     first))))

(defn day-2-2
  [input]
  (first
   (for [noun (range 0 99)
         verb (range 0 99)
         :let [updated-input (-> input
                                 (assoc 1 noun)
                                 (assoc 2 verb))]
         :when (= 19690720 (first (day-2 updated-input)))]
     (+ (* 100 noun) verb))))

(deftest day-2-2-test
  (is (= 9425 (day-2-2 (read-syms (slurp "day-2.txt"))))))

(defn right [[x y]] [(inc x) y])
(defn up    [[x y]] [x (inc y)])
(defn left  [[x y]] [(dec x) y])
(defn down  [[x y]] [x (dec y)])

(def get-move-fn
  {\R right
   \U up
   \L left
   \D down})

(defn read-move
  [[dir & chars]]
  (repeat (Integer/parseInt (str/join chars)) (get-move-fn dir)))

(defn read-path
  [s]
  (mapcat read-move (str/split s #",")))

(defn eval-path
  [start path]
  (reductions (fn [pos move-fn]
                (move-fn pos))
              start
              path))

(defn find-intersections
  [p1 p2]
  (disj (set/intersection (set p1) (set p2))
        [0 0]))

(defn day-3
  [input]
  (let [[w1 w2] (str/split-lines input)
        p1 (eval-path [0 0] (read-path w1))
        p2 (eval-path [0 0] (read-path w2))
        distances (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))) (find-intersections p1 p2))]
    (apply min distances)))

(deftest day-3-test
  (is (= (repeat 2 right) (read-move "R2")))
  (is (= [up up down down left right left right] (read-path "U2,D2,L1,R1,L1,R1")))
  (is (= [[0 0] [0 1] [0 2] [0 1] [0 0] [-1 0] [0 0] [-1 0] [0 0]]
         (eval-path [0 0] (read-path "U2,D2,L1,R1,L1,R1"))))
  (is (= 159 (day-3 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")))
  (is (= 135 (day-3 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))
  (is (= 260 (day-3 (slurp "day-3.txt")))))

(defn intersection-distances
  [intersections path]
  (let [result (->> path
                  (map-indexed vector)
                  (filter #(contains? intersections (second %))))]
    (assert (= (count intersections) (count result))
            "Danger: intersections were crossed multiple times.")
    result))

(defn day-3-2
  [input]
  (let [[w1 w2] (str/split-lines input)
        p1 (eval-path [0 0] (read-path w1))
        p2 (eval-path [0 0] (read-path w2))
        intersections (find-intersections p1 p2)
        p1-distances (intersection-distances intersections p1)
        p2-distances (intersection-distances intersections p2)]
    (apply min
           (for [[dist1 loc1] p1-distances
                 [dist2 loc2] p2-distances
                 :when (= loc1 loc2)]
             (+ dist1 dist2)))))

(deftest day-3-2-test
  (is (= 610 (day-3-2 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")))
  (is (= 410 (day-3-2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))
  (is (= 15612 (day-3-2 (slurp "day-3.txt")))))

(def char->int
  (reduce (fn [m n]
            (assoc m (first (str n)) n))
          {}
          (range 10)))

(defn day-4
  [n]
  (let [s (str n)
        digits (mapv char->int s)
        six-digits? (= 6 (count digits))
        has-adjacent? (some true? (map = digits (rest digits)))
        never-decreases? (reduce (fn [a b]
                                   (and a (<= a b) b))
                                 digits)]
    (and six-digits?
         has-adjacent?
         never-decreases?)))

(deftest day-4-test
  (is (day-4 111111))
  (is (not (day-4 223450)))
  (is (not (day-4 123789)))
  (is (not (day-4 111101)))
  (is (= 2 (count (filter day-4 (range 122343 122346)))))
  (is (= 511 (count (filter day-4 (range 359282 820401))))))

(defn day-4-2
  [n]
  (let [s (str n)
        digits (mapv char->int s)
        six-digits? (= 6 (count digits))
        has-adjacent? (->> digits
                           frequencies
                           vals
                           (some #(= 2 %)))
        never-decreases? (reduce (fn [a b]
                                   (and a (<= a b) b))
                                 digits)]
    (and six-digits?
         has-adjacent?
         never-decreases?)))

(deftest day-4-2-test
  (is (day-4-2 112233))
  (is (not (day-4-2 123444)))
  (is (day-4-2 111122))
  (is (= 316 (count (filter day-4-2 (range 359282 820401))))))

(defn read-instruction
  "Given a number, returns the last two digits as a num, then each subsequent
  digit working backwards followed by an infinite number of zeros."
  [inst]
  (map first
       (iterate (fn [[_ n]]
                  [(rem n 10) (quot n 10)])
                [(rem inst 100) (quot inst 100)])))

(defn read-expr
  [{:keys [pos data] :as env}]
  (let [[op & param-modes] (read-instruction (get data pos))
        op->arity {1 3
                   2 3
                   3 1
                   4 1
                   5 2
                   6 2
                   7 3
                   8 3
                   99 0}
        arity (op->arity op)
        _ (when (nil? arity)
            (throw (Exception. (str "Arity not found for op " op))))
        arg-loaders (take arity
                          (map {0 #(get data %)
                                1 identity}
                               param-modes))
        arg-loaders (->> param-modes
                         (map {0 #(get data %)
                               1 identity})
                         (take arity))
        raw-args (->> data
                      ;; using inc to also skip the instruction
                      (drop (inc pos))
                      (take arity))
        loaded-args (map #(%1 %2) arg-loaders raw-args)]
    {:op op
     :arity arity
     :raw-args raw-args
     :loaded-args loaded-args}))

(defn eval-expr
  [{:keys [pos data in out] :as env} {:keys [op arity raw-args loaded-args] :as expr}]
  (let [next-pos (+ pos (inc arity))
        updates (case op
                  ;; add
                  1 (let [[a b] loaded-args
                          dest (last raw-args)]
                      {:data (assoc data dest (+ a b))})
                  ;; mult
                  2 (let [[a b] loaded-args
                          dest (last raw-args)]
                      {:data (assoc data dest (* a b))})
                  ;; read
                  3 (let [[dest] raw-args]
                      {:in (pop in)
                       :data (assoc data dest (peek in))})
                  ;; write
                  4 (let [[value] loaded-args]
                      {:out (conj out value)})
                  ;; jump-if-true
                  5 (let [[test jump-pos] loaded-args]
                      (when-not (zero? test) {:pos jump-pos}))
                  ;; jump-if-else
                  6 (let [[test jump-pos] loaded-args]
                      (when (zero? test) {:pos jump-pos}))
                  ;; less than
                  7 (let [[a b] loaded-args
                          dest (last raw-args)]
                      {:data (assoc data dest (if (< a b) 1 0))})
                  ;; equal
                  8 (let [[a b] loaded-args
                          dest (last raw-args)]
                      {:data (assoc data dest (if (= a b) 1 0))})
                  ;; quit
                  99 {:done true}
                  (throw (Exception. (str "Unrecognized op for expression: " expr))))]
    (merge env {:pos next-pos} updates)))

(defn day-5
  ([program] (day-5 program []))
  ([program input]
   (loop [env {:pos 0
               :data program
               :in (into (clojure.lang.PersistentQueue/EMPTY) input)
               :out (clojure.lang.PersistentQueue/EMPTY)}]
     (if (:done env)
       env
       (recur (eval-expr env (read-expr env)))))))

(deftest day-5-test
  (testing "day-2 tests"
    (is (= [2,0,0,0,99] (:data (day-5 (read-syms "1,0,0,0,99")))))
    (is (= [2,3,0,6,99] (:data (day-5 (read-syms "2,3,0,3,99")))))
    (is (= [2,4,4,5,99,9801] (:data (day-5 (read-syms "2,4,4,5,99,0")))))
    (is (= [30,1,1,4,2,5,6,0,99] (:data (day-5 (read-syms "1,1,1,4,99,5,6,0,99")))))
    (is (= 3085697 (-> (slurp "day-2.txt")
                       read-syms
                       (assoc 1 12)
                       (assoc 2 2)
                       day-5
                       :data
                       first))))
  (testing "part one"
    (is (= [1234] (:out (day-5 (read-syms "3,0,4,0,99") [1234]))))
    (is (= [2 0 1 0] (take 4 (read-instruction 1002))))
    (is (= [1002 4 3 4 99] (:data (day-5 (read-syms "1002,4,3,4,33")))))
    (is (= [11002 4 3 4 99] (:data (day-5 (read-syms "11002,4,3,4,33")))))
    (is (= [1101 100 -1 4 99] (:data (day-5 (read-syms "1101,100,-1,4,0")))))
    (is (->> (day-5 (read-syms (slurp "day-5.txt")) [1])
             :out
             butlast
             (every? zero?)))
    (is (= 16574641 (-> (day-5 (read-syms (slurp "day-5.txt")) [1])
                        :out
                        last))))
  (testing "part two"
    (testing "input equals 8"
      (is (= 1 (first (:out (day-5 (read-syms "3,9,8,9,10,9,4,9,99,-1,8") [8])))))
      (is (= 0 (first (:out (day-5 (read-syms "3,9,8,9,10,9,4,9,99,-1,8") [7])))))
      (is (= 1 (first (:out (day-5 (read-syms "3,3,1108,-1,8,3,4,3,99") [8])))))
      (is (= 0 (first (:out (day-5 (read-syms "3,3,1108,-1,8,3,4,3,99") [7]))))))
    (testing "input is less than 8"
      (is (= 1 (first (:out (day-5 (read-syms "3,9,7,9,10,9,4,9,99,-1,8") [7])))))
      (is (= 0 (first (:out (day-5 (read-syms "3,9,7,9,10,9,4,9,99,-1,8") [8])))))
      (is (= 1 (first (:out (day-5 (read-syms "3,3,1107,-1,8,3,4,3,99") [7])))))
      (is (= 0 (first (:out (day-5 (read-syms "3,3,1107,-1,8,3,4,3,99") [8]))))))
    (testing "jump-tests; test if input is non-zero"
      (is (= 1 (first (:out (day-5 (read-syms "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") [8])))))
      (is (= 0 (first (:out (day-5 (read-syms "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") [0])))))
      (is (= 1 (first (:out (day-5 (read-syms "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") [8])))))
      (is (= 0 (first (:out (day-5 (read-syms "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") [0]))))))
    (testing "larger example that is a funny comparator for the number 8"
      (let [program (read-syms "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")]
        (is (= 999 (first (:out (day-5 program [7])))))
        (is (= 1000 (first (:out (day-5 program [8])))))
        (is (= 1001 (first (:out (day-5 program [9])))))))
    (is (= 15163975 (first (:out (day-5 (read-syms (slurp "day-5.txt")) [5])))))))

(defn parse-orbital-map
  [input]
  (->> (str/split input #"[\)\s]+")
       (partition 2)
       (mapv (comp vec reverse))
       (into {})))

(defn count-ancestors
  [get-parent node]
  (->> node
       (iterate get-parent)
       rest
       (take-while some?)
       count))

(defn day-6
  [input]
  (let [child->parent (parse-orbital-map input)
        satellites (keys child->parent)]
    (->> satellites
         (map #(count-ancestors child->parent %))
         (reduce +))))

(deftest day-6-test
  (is (= 42 (day-6 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")))
  (is (= 1 (day-6 "COM)97W")))
  (is (= 142497 (day-6 (slurp "day-6.txt")))))

(defn drop-while-same
  [[a & a-tail :as as] [b & b-tail :as bs]]
  (if (= a b)
    (recur a-tail b-tail)
    [as bs]))

(defn day-6-2
  [input]
  (let [child->parent (parse-orbital-map input)
        you-path (reverse (take-while some? (iterate child->parent "YOU")))
        san-path (reverse (take-while some? (iterate child->parent "SAN")))]
    (->> (drop-while-same (butlast you-path) (butlast san-path))
         (apply concat)
         count)))

(deftest day-6-2-test
  (is (= 4 (day-6-2 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")))
  (is (= 301 (day-6-2 (slurp "day-6.txt")))))
