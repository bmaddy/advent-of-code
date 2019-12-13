(ns aoc-2019
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
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
  (is (= 33583 (day-1-1 "100756"))))
#_(day-1-1 (slurp "day-1.txt"))

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
  (is (= 50346 (day-1-2 "100756"))))
#_(day-1-2 (slurp "day-1.txt"))

(defn run-op
  [data f k1 k2 out]
  (let [a (get data k1)
        b (get data k2)]
    (assoc data out (f a b))))

(defn read-nums
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
  (is (= [2,0,0,0,99] (day-2 (read-nums "1,0,0,0,99"))))
  (is (= [2,3,0,6,99] (day-2 (read-nums "2,3,0,3,99"))))
  (is (= [2,4,4,5,99,9801] (day-2 (read-nums "2,4,4,5,99,0"))))
  (is (= [30,1,1,4,2,5,6,0,99] (day-2 (read-nums "1,1,1,4,99,5,6,0,99")))))
#_(-> (slurp "day-2.txt")
      read-nums
      (assoc 1 12)
      (assoc 2 2)
      day-2
      first)

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
#_(day-2-2 (read-nums (slurp "day-2.txt")))

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
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))))
#_(day-3 (slurp "day-3.txt"))

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
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))))
#_(day-3-2 (slurp "day-3.txt"))

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
  (is (= 2 (count (filter day-4 (range 122343 122346))))))
#_(count (filter day-4 (range 359282 820401)))

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
  (is (day-4-2 111122)))
#_(count (filter day-4-2 (range 359282 820401)))

(defn read-expr
  [{:keys [pos data] :as env}]
  (let [op (get data pos)
        num-args {1 3
                  2 3
                  3 1
                  4 1
                  99 0}]
    (take (inc (get num-args op)) (drop pos data))))

(defn eval-expr
  [{:keys [pos data] :as env} [op & args :as expr]]
  (case op
    1 (let [[k1 k2 out] args]
        (assoc env
               :data (run-op data + k1 k2 out)
               :pos (+ pos 4)))
    2 (let [[k1 k2 out] args]
        (assoc env
               :data (run-op data * k1 k2 out)
               :pos (+ pos 4)))
    99 (assoc env :done true)
    (throw (Exception. (str "Unrecognized expression: " expr)))))

(defn day-5
  [input]
  (loop [env {:pos 0
              :data input}]
    (if (:done env)
      (:data env)
      (recur (eval-expr env (read-expr env))))))

(deftest day-5-test
  (is (= [2,0,0,0,99] (day-5 (read-nums "1,0,0,0,99"))))
  (is (= [2,3,0,6,99] (day-5 (read-nums "2,3,0,3,99"))))
  (is (= [2,4,4,5,99,9801] (day-5 (read-nums "2,4,4,5,99,0"))))
  (is (= [30,1,1,4,2,5,6,0,99] (day-5 (read-nums "1,1,1,4,99,5,6,0,99"))))
  (is (= 3085697 (-> (slurp "day-2.txt")
                     read-nums
                     (assoc 1 12)
                     (assoc 2 2)
                     day-5
                     first))))
