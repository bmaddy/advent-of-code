(ns intcode
  (:require [clojure.edn :as edn]))

(defn read-syms
  [s]
  (edn/read-string (str "[" s "]"))
  #_(reduce-kv assoc {} (edn/read-string (str "[" s "]"))))

(defn make-env
  [program input]
  {:pos 0
   :data program
   :relative-base 0
   :in (into (clojure.lang.PersistentQueue/EMPTY) input)
   :out (clojure.lang.PersistentQueue/EMPTY)})

(defn read-instruction
  "Given a number, returns the last two digits as a num, then each subsequent
  digit working backwards followed by an infinite number of zeros."
  [inst]
  (assert inst (str "invalid instruction: " (pr-str inst)))
  (->> [(rem inst 100) (quot inst 100)]
       (iterate (fn [[_ n]]
                  [(rem n 10) (quot n 10)]))
       (map first)))

(defn read-expr
  [{:keys [pos data relative-base] :as env}]
  (let [[op & param-modes] (read-instruction (get data pos))
        op->arity {1 3
                   2 3
                   3 1
                   4 1
                   5 2
                   6 2
                   7 3
                   8 3
                   9 1
                   99 0}
        arity (op->arity op)
        _ (when (nil? arity)
            (throw (Exception. (str "Arity not found for op " op))))
        arg-loaders (->> param-modes
                         (map {;; position mode
                               0 #(get data %)
                               ;; immediate mode
                               1 identity
                               ;; relative mode
                               2 #(get data (+ relative-base %))})
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
  [{:keys [pos data relative-base in out] :as env}
   {:keys [op arity raw-args loaded-args] :as expr}]
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
                      (assert (not-empty in) "Tried to read from empty input.")
                      {:data (assoc data dest (peek in))
                       :in (pop in)})
                  ;; write
                  4 (let [[value] loaded-args]
                      {:out (conj out value)})
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
                  ;; adjust relative-base
                  9 (let [[value] loaded-args]
                      {:relative-base (+ relative-base value)})
                  ;; quit
                  99 {:done true}
                  (throw (Exception. (str "Unrecognized op for expression: " expr))))]
    (merge env {:pos next-pos} updates)))

(defn run
  ([program] (run program []))
  ([program input]
   (loop [env (make-env (if (string? program)
                          (read-syms program)
                          program)
                        input)]
     (if (:done env)
       env
       (recur (eval-expr env (read-expr env)))))))
