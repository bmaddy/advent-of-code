(ns aoc-2019
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is run-tests testing]]
            [clojure.set :as set]
            [clojure.tools.trace :refer [deftrace]]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a :refer [go go-loop chan <! >! <!! >!!]]
            [intcode :as int]
            [sc.api]))

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
  (assert inst (str "invalid instruction: " (pr-str inst)))
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

(defn series-circuit
  [program phases input]
  (if (empty? phases)
    input
    (let [[phase & remaining-phases] phases
          output (-> (day-5 program [phase input])
                     :out
                     first)]
      (recur program remaining-phases output))))

(defn day-7
  [input]
  (->> (range 5)
       combo/permutations
       (mapv (fn [phases]
               {:phases phases
                :output (series-circuit input phases 0)}))
       (reduce #(max-key :output %1 %2))))

(deftest day-7-test
  (is (= {:phases [4 3 2 1 0]
          :output 43210} (day-7 (read-syms "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))))
  (is (= {:phases [0 1 2 3 4]
          :output 54321} (day-7 (read-syms "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0"))))
  (is (= {:phases [1 0 4 3 2]
          :output 65210} (day-7 (read-syms "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))))
  (is (= 17406 (:output (day-7 (read-syms (slurp "day-7.txt")))))))

(defn make-env
  [id program]
  {:id id
   :pos 0
   :data program
   :in (chan)
   :out (chan)})

(defn eval-expr-ch
  [{:keys [id pos data in out] :as env} {:keys [op arity raw-args loaded-args] :as expr}]
  (go
    (let [next-pos (+ pos (inc arity))
          quit (fn []
                 (a/close! in)
                 (a/close! out)
                 {:pos pos
                  :done true})
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
                    3 (let [[dest] raw-args
                            ;; _ (println (str id " reading from in..."))
                            input (<! in)]
                        ;; (println (str input " -> " id))
                        (if (nil? input)
                          (quit)
                          {:data (assoc data dest input)}))
                    ;; write
                    4 (let [[value] loaded-args]
                        ;; (println (str id " -> " value))
                        (>! out value)
                        {})
                    ;; jump-if-true
                    5 (let [[test jump-pos] loaded-args]
                        (when-not (zero? test)
                          {:pos jump-pos}))
                    ;; jump-if-else
                    6 (let [[test jump-pos] loaded-args]
                        (when (zero? test)
                          {:pos jump-pos}))
                    ;; less than
                    7 (let [[a b] loaded-args
                            dest (last raw-args)]
                        {:data (assoc data dest (if (< a b) 1 0))})
                    ;; equal
                    8 (let [[a b] loaded-args
                            dest (last raw-args)]
                        {:data (assoc data dest (if (= a b) 1 0))})
                    ;; quit
                    99 (quit)
                    (throw (Exception. (str "Unrecognized op for expression: " expr))))]
      (merge env {:pos next-pos} updates))))

(defn make-computer
  ([program] (make-computer (gensym) program))
  ([id program]
   (let [{:keys [in out] :as env} (make-env id program)
         done-ch (chan)]
     (go-loop [env env]
       (if (:done env)
         (do (>! done-ch env)
             (a/close! done-ch))
         (recur (<! (eval-expr-ch env (read-expr env))))))
     [in out done-ch])))

(defn run-feedback-loop
  [program phases]
  (let [[inputs outputs done-chs] (->> [:a :b :c :d :e]
                                       (mapv #(make-computer % program))
                                       (apply mapv vector))
        a-in (first inputs)
        e-out (last outputs)
        all-from-e (chan (a/sliding-buffer 1))]
    ;; connect outputs to inputs (except for the last output and first input)
    (mapv #(a/pipe %1 %2) outputs (rest inputs))
    ;; remember latest output from e and pass along to a
    (go-loop []
      (if-let [value (<! e-out)]
        (do
          (>! all-from-e value)
          (>! a-in value)
          (recur))
        (do
          (a/close! all-from-e)
          (a/close! a-in))))
    ;; set phases as initial inputs
    (mapv #(>!! %1 %2) inputs phases)
    ;; start processing by supplying the first input to a
    (>!! a-in 0)
    ;; wait until any computer stops before returning the last result from e
    (a/alts!! done-chs)
    (last (<!! (a/into [] all-from-e)))))

(defn run-computer
  ([s] (run-computer s []))
  ([s inputs]
   (let [[in-ch out-ch done-ch] (make-computer (if (string? s) (read-syms s) s))]
     (a/onto-chan in-ch inputs)
     (let [out (<!! (a/into [] out-ch))]
       (assoc (<!! done-ch) :out out)))))

(defn day-7-2
  [input]
  (->> (range 5 10)
       combo/permutations
       (mapv (fn [phases]
               {:phases phases
                :output (run-feedback-loop (read-syms input) phases)}))
       (reduce #(max-key :output %1 %2))))

(deftest day-7-2-test
  (testing "day-2 tests"
    (is (= [2,0,0,0,99] (:data (run-computer "1,0,0,0,99"))))
    (is (= [2,3,0,6,99] (:data (run-computer "2,3,0,3,99"))))
    (is (= [2,4,4,5,99,9801] (:data (run-computer "2,4,4,5,99,0"))))
    (is (= [30,1,1,4,2,5,6,0,99] (:data (run-computer "1,1,1,4,99,5,6,0,99"))))
    (is (= 3085697 (-> (slurp "day-2.txt")
                       read-syms
                       (assoc 1 12)
                       (assoc 2 2)
                       run-computer
                       :data
                       first))))

  (testing "day-5 tests"
    (testing "part one"
      (is (= [1234] (:out (run-computer "3,0,4,0,99" [1234]))))
      (is (= [2 0 1 0] (take 4 (read-instruction 1002))))
      (is (= [1002 4 3 4 99] (:data (run-computer "1002,4,3,4,33"))))
      (is (= [11002 4 3 4 99] (:data (run-computer "11002,4,3,4,33"))))
      (is (= [1101 100 -1 4 99] (:data (run-computer "1101,100,-1,4,0"))))
      (is (->> (run-computer (slurp "day-5.txt") [1])
               :out
               butlast
               (every? zero?)))
      (is (= 16574641 (-> (run-computer (slurp "day-5.txt") [1])
                          :out
                          last))))
    (testing "part two"
      (testing "input equals 8"
        (is (= 1 (first (:out (run-computer "3,9,8,9,10,9,4,9,99,-1,8" [8])))))
        (is (= 0 (first (:out (run-computer "3,9,8,9,10,9,4,9,99,-1,8" [7])))))
        (is (= 1 (first (:out (run-computer "3,3,1108,-1,8,3,4,3,99" [8])))))
        (is (= 0 (first (:out (run-computer "3,3,1108,-1,8,3,4,3,99" [7]))))))
      (testing "input is less than 8"
        (is (= 1 (first (:out (run-computer "3,9,7,9,10,9,4,9,99,-1,8" [7])))))
        (is (= 0 (first (:out (run-computer "3,9,7,9,10,9,4,9,99,-1,8" [8])))))
        (is (= 1 (first (:out (run-computer "3,3,1107,-1,8,3,4,3,99" [7])))))
        (is (= 0 (first (:out (run-computer "3,3,1107,-1,8,3,4,3,99" [8]))))))
      (testing "jump-tests; test if input is non-zero"
        (is (= 1 (first (:out (run-computer "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [8])))))
        (is (= 0 (first (:out (run-computer "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" [0])))))
        (is (= 1 (first (:out (run-computer "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [8])))))
        (is (= 0 (first (:out (run-computer "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" [0]))))))
      (testing "larger example that is a funny comparator for the number 8"
        (let [program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
          (is (= 999 (first (:out (run-computer program [7])))))
          (is (= 1000 (first (:out (run-computer program [8])))))
          (is (= 1001 (first (:out (run-computer program [9])))))))
      (is (= 15163975 (first (:out (run-computer (slurp "day-5.txt") [5])))))))

  (testing "day-7-2 tests"
    (is (= 139629729
           (run-feedback-loop (read-syms "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
                              [9,8,7,6,5])))
    (is (= {:phases [9,8,7,6,5] :output 139629729}
           (day-7-2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")))
    (is (= 18216
           (run-feedback-loop (read-syms "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
                              [9,7,8,5,6])))
    (is (= {:phases [9,7,8,5,6] :output 18216}
           (day-7-2 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")))
    (is (= {:phases [7 8 6 9 5], :output 1047153}
           (day-7-2 (slurp "day-7.txt"))))))

(defn day-8
  [input w h]
  (let [counts (->> input
                   (partition (* w h))
                   (mapv frequencies)
                   (apply min-key #(get % \0 0)))]
    (* (get counts \1 0)
       (get counts \2 0))))

(deftest day-8-test
  (is (= 1 (day-8 "123456789012" 3 2)))
  (is (= 1560 (day-8 (slurp "day-8.txt") 25 6))))

(defn day-8-2
  [input w h]
  (->> input
       (partition (* w h))
       (reduce (fn [a b]
                 (mapv (fn [x y]
                         (if (= x \2) y x))
                       a b)))
       (mapv {\1 \X
              \0 \space
              \2 \space})
       (partition w)
       (mapv str/join)
       (str/join \newline)))

(deftest day-8-2-test
  (is (= " X\nX " (day-8-2 "0222112222120000" 2 2)))
  (is (= ["X  X  XX   XX  X  X X  X "
          "X  X X  X X  X X  X X  X "
          "X  X X    X    X  X XXXX "
          "X  X X XX X    X  X X  X "
          "X  X X  X X  X X  X X  X "
          " XX   XXX  XX   XX  X  X "]
         (str/split (day-8-2 (slurp "day-8.txt") 25 6) #"\n"))))

(defn char-locs
  [chars input]
  (set
   (for [[y row] (map-indexed vector (str/split-lines input))
         [x item] (map-indexed vector row)
         :when (contains? chars item)]
     [x y])))

(defn build-edges
  [asteroids]
  (for [[src-x src-y :as src] asteroids
        [dest-x dest-y :as dest] (disj asteroids src)
        :let [dx (- dest-x src-x)
              dy (- dest-y src-y)
              gcd (.gcd (biginteger dx) (biginteger dy))
              reduced-x (/ dx gcd)
              reduced-y (/ dy gcd)]]
    {:src src
     :dest dest
     :relative-loc [dx dy]
     :direction [reduced-x reduced-y]}))

(defn add-reverse-edges
  [edges]
  (mapcat (fn [{:keys [src dest relative-loc direction] :as edge}]
            [edge
             {:src dest
              :dest src
              :relative-loc (mapv - relative-loc)
              :direction (mapv - direction)}])
          edges))

(defn visible-asteroids
  [input]
  (->> input
       (char-locs #{\#})
       build-edges
       add-reverse-edges
       (group-by (juxt :src :direction))
       (map ffirst)
       frequencies))

(defn day-10
  [input]
  (reduce max (vals (visible-asteroids input))))

(deftest day-10-test
  (let [m ".#..#
.....
#####
....#
...##"]
    (is (= {[0 2] 6
            [1 0] 7
            [1 2] 7
            [2 2] 7
            [3 2] 7
            [3 4] 8
            [4 0] 7
            [4 2] 5
            [4 3] 7
            [4 4] 7}
           (visible-asteroids m)))
    (is (= 8 (day-10 m))))

  (let [m "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####"]
    (is (= 33 (get (visible-asteroids m) [5 8])))
    (is (= 33 (day-10 m))))

  (let [m "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###."]
    (is (= 35 (get (visible-asteroids m) [1 2])))
    (is (= 35 (day-10 m))))

  (let [m ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#.."]
    (is (= 41 (get (visible-asteroids m) [6 3])))
    (is (= 41 (day-10 m))))
  (let [m ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"]
    (is (= 210 (get (visible-asteroids m) [11 13])))
    (is (= 210 (day-10 m))))

  (is (= 278 (day-10 (slurp "day-10.txt")))))

(defn angle-from-north
  [[dx dy]]
  (let [+x (Math/abs (int dx))
        +y (Math/abs (int dy))]
    (condp = [(compare dx 0) (compare dy 0)]
      [0 -1] 0
      ;; top right
      [1 -1] (Math/atan (/ +x +y))
      [1 0] (* 1/2 Math/PI)
      ;; bottom right
      [1 1] (+ (* 1/2 Math/PI) (Math/atan (/ +y +x)))
      [0 1] Math/PI
      ;; bottom left
      [-1 1] (+ Math/PI (Math/atan (/ +x +y)))
      [-1 0] (* 3/2 Math/PI)
      ;; top left
      [-1 -1] (+ (* 3/2 Math/PI) (Math/atan (/ +y +x))))))

(defn manhattan-dist
  [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(defn round-robin-seq
  [colls]
  (when-not (empty? colls)
      (into (mapv first colls)
            (round-robin-seq (remove nil? (mapv next colls))))))

(defn day-10-2
  [station m]
  (let [edges-by-angle (->> m
                            (char-locs (set "#X"))
                            build-edges
                            (filter #(= station (:src %)))
                            (mapv #(assoc %
                                          :angle (angle-from-north (:direction %))
                                          :distance (manhattan-dist (:relative-loc %))))
                            (group-by :angle)
                            ;; sort by angle from North to the key (:direction)
                            (sort-by first)
                            vals
                            ;; sort each row so the closest is first
                            (mapv #(sort-by :distance %)))]
    ;; read first in round-robbin fashion
    (mapv :dest (round-robin-seq edges-by-angle))))

(deftest day-10-2-test
  (is (= 0 (angle-from-north [0 -1])))
  (is (= (Math/toRadians 45) (angle-from-north [1 -1])))
  (is (= (Math/toRadians 90) (angle-from-north [1 0])))
  (is (= (Math/toRadians 135) (angle-from-north [1 1])))
  (is (= (Math/toRadians 180) (angle-from-north [0 1])))
  (is (= (Math/toRadians 225) (angle-from-north [-1 1])))
  (is (= (Math/toRadians 270) (angle-from-north [-1 0])))
  (is (= (Math/toRadians 315) (angle-from-north [-1 -1])))
  (let [m ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##"
        first-nine ".#....###24...#..
##...##.13#67..9#
##...#...5.8####.
..#.....X...###..
..#.#.....#....##"
        second-nine ".#....###.....#..
##...##...#.....#
##...#......1234.
..#.....X...5##..
..#.9.....8....76"
        third-nine ".8....###.....#..
56...9#...#.....#
34...7...........
..2.....X....##..
..1.............."
        fourth-nine "......234.....6..
......1...5.....7
.................
........X....89..
................."
        station (first (char-locs #{\X} m))
        first-expected (mapv #(first (char-locs #{%} first-nine)) "123456789")
        second-expected (mapv #(first (char-locs #{%} second-nine)) "123456789")
        third-expected (mapv #(first (char-locs #{%} third-nine)) "123456789")
        fourth-expected (mapv #(first (char-locs #{%} fourth-nine)) "123456789")
        result (day-10-2 station m)]
    (is (= first-expected (take 9 result)))
    (is (= second-expected (take 9 (drop 9 result))))
    (is (= third-expected (take 9 (drop 18 result))))
    (is (= fourth-expected (take 9 (drop 27 result)))))
  (let [m ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"
        order (day-10-2 [11 13] m)]
    (is (= [[11 12] [12 1] [12 2]] (take 3 order)))
    (is (= [12 8] (nth order (dec 10))))
    (is (= [16 0] (nth order (dec 20))))
    (is (= [16 9] (nth order (dec 50))))
    (is (= [10 16] (nth order (dec 100))))
    (is (= [9 6] (nth order (dec 199))))
    (is (= [8 2] (nth order (dec 200))))
    (is (= [10 9] (nth order (dec 201))))
    (is (= [11 1]
           (nth order 298)
           (last order)))
    (is (= [14 17] (nth (day-10-2 [23 19] (slurp "day-10.txt")) (dec 200))))))

(def turn-left
  {[0 -1] [-1 0]
   [-1 0] [0 1]
   [0 1] [1 0]
   [1 0] [0 -1]})

(def turn-right
  (set/map-invert turn-left))

(defn robot-step
  [{:keys [pos orientation white-panels env] :as state}]
  ;; (prn `(robot-step ~state))
  (let [input (if (contains? white-panels pos) 1 0)
        _ (sc.api/spy)                  ;
        new-env (->> (update env :in conj input)
                     (iterate int/step)
                     (drop-while #(-> % :out pop peek nil?))
                     (map #(dissoc % :data))
                     first)
        [color-id turn-id] (take 2 (:out new-env))
        new-orientation (if (zero? turn-id)
                          (turn-left orientation)
                          (turn-right orientation))
        new-pos (mapv + pos new-orientation)]
    (assoc state
           :pos new-pos
           :orientation new-orientation
           :white-panels (if (zero? color-id)
                           (disj white-panels pos)
                           (conj white-panels pos))
           :env (update new-env :out (comp pop pop)))))

(defn day-11
  [program]
  (let [state {:pos [0 0]
               :orientation [0 -1]
               :white-panels #{}
               :env (int/make-env (int/read-syms program) [])}]
    (loop [{:keys [env] :as state} state
           states []]
      (if (:done env)
        (->> states (map :pos) (into #{}) count)
        (recur (robot-step state) (conj states state))))))

(deftest day-11-test
  (let [states
        [{:orientation [0 -1] :env {:in [] :out [1 0]} :pos [0 0] :white-panels #{}}
         {:orientation [-1 0] :env {:in [] :out [0 0]} :pos [-1 0] :white-panels #{[0 0]}}
         {:orientation [0 1] :env {:in [] :out [1 0]} :pos [-1 1] :white-panels #{[0 0]}}
         {:orientation [1 0] :env {:in [] :out [1 0]} :pos [0 1] :white-panels #{[0 0] [-1 1]}}
         {:orientation [0 -1] :env {:in [] :out [0 1]} :pos [0 0] :white-panels #{[0 0] [-1 1]
                                                                                  [0 1]}}
         {:orientation [1 0] :env {:in [] :out [1 0]} :pos [1 0] :white-panels #{[-1 1] [0 1]}}
         {:orientation [0 -1] :env {:in [] :out [1 0]} :pos [1 -1] :white-panels #{[-1 1] [0 1]
                                                                                   [1 0]}}
         {:orientation [-1 0] :env {:in [] :out []} :pos [0 -1] :white-panels #{[-1 1] [0 1]
                                                                                [1 0] [1 -1]}}]
        ks [:pos :orientation :white-panels]]
    (with-redefs [int/step identity]
      (is (= (-> states second (select-keys ks)) (select-keys (robot-step (first states)) ks)))
      (is (= (-> states (nth 2) (select-keys ks)) (select-keys (robot-step (second states)) ks)))
      (is (= (-> states (nth 3) (select-keys ks)) (select-keys (robot-step (nth states 2)) ks)))
      (is (= (-> states (nth 4) (select-keys ks)) (select-keys (robot-step (nth states 3)) ks)))
      (is (= (-> states (nth 5) (select-keys ks)) (select-keys (robot-step (nth states 4)) ks)))
      (is (= (-> states (nth 6) (select-keys ks)) (select-keys (robot-step (nth states 5)) ks)))
      (is (= (-> states last (select-keys ks)) (select-keys (robot-step (nth states 6)) ks))))
    (is (= 6 (->> states butlast (map :pos) (into #{}) count))))
  #_(is (= 0 (day-11 (slurp "day-11.txt")))))
