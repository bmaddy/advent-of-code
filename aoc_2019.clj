(ns aoc-2019
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]))

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
