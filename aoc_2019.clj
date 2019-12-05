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
