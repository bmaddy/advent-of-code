(ns intcode-test
  (:require [intcode :as sut]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest read-instruction-test
  (is (= [2 0 1 0] (take 4 (sut/read-instruction 1002)))))

(deftest run-test
  (testing "day-2"
    (is (= [2,0,0,0,99] (:data (sut/run "1,0,0,0,99"))))
    (is (= [2,3,0,6,99] (:data (sut/run "2,3,0,3,99"))))
    (is (= [2,4,4,5,99,9801] (:data (sut/run "2,4,4,5,99,0"))))
    (is (= [30,1,1,4,2,5,6,0,99] (:data (sut/run "1,1,1,4,99,5,6,0,99"))))
    (is (= 3085697 (-> (slurp "day-2.txt")
                       sut/read-syms
                       (assoc 1 12)
                       (assoc 2 2)
                       sut/run
                       :data
                       first))))

  (testing "day-5"
    (testing "part one"
      (is (= [1234] (:out (sut/run "3,0,4,0,99" [1234]))))
      (is (= [1002 4 3 4 99] (:data (sut/run "1002,4,3,4,33"))))
      (is (= [11002 4 3 4 99] (:data (sut/run "11002,4,3,4,33"))))
      (is (= [1101 100 -1 4 99] (:data (sut/run "1101,100,-1,4,0"))))
      (is (->> (sut/run (slurp "day-5.txt") [1])
               :out
               butlast
               (every? zero?)))
      (is (= 16574641 (-> (sut/run (slurp "day-5.txt") [1])
                          :out
                          last))))
    (testing "part two"
      (testing "input equals 8"
        (is (= 1 (first (:out (sut/run "3,9,8,9,10,9,4,9,99,-1,8" [8])))))
        (is (= 0 (first (:out (sut/run "3,9,8,9,10,9,4,9,99,-1,8" [7])))))
        (is (= 1 (first (:out (sut/run "3,3,1108,-1,8,3,4,3,99" [8])))))
        (is (= 0 (first (:out (sut/run "3,3,1108,-1,8,3,4,3,99" [7]))))))
      (testing "input is less than 8"
        (is (= 1 (first (:out (sut/run "3,9,7,9,10,9,4,9,99,-1,8" [7])))))
        (is (= 0 (first (:out (sut/run "3,9,7,9,10,9,4,9,99,-1,8" [8])))))
        (is (= 1 (first (:out (sut/run "3,3,1107,-1,8,3,4,3,99" [7])))))
        (is (= 0 (first (:out (sut/run "3,3,1107,-1,8,3,4,3,99" [8]))))))
      (testing "jump-tests; test if input is non-zero"
        (is (= 1 (first (:out (sut/run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [8])))))
        (is (= 0 (first (:out (sut/run "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [0])))))
        (is (= 1 (first (:out (sut/run "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [8])))))
        (is (= 0 (first (:out (sut/run "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [0]))))))
      (testing "larger example that is a funny comparator for the number 8"
        (let [program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
          (is (= 999 (first (:out (sut/run program [7])))))
          (is (= 1000 (first (:out (sut/run program [8])))))
          (is (= 1001 (first (:out (sut/run program [9])))))))
      (is (= 15163975 (first (:out (sut/run (slurp "day-5.txt") [5])))))))

  (testing "day-9"
      (is (= [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
             (:out (sut/run "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))))))
