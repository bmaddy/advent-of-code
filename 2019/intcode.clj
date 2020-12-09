(ns intcode
  (:require [clojure.edn :as edn]))

(defn read-syms
  [s]
  (edn/read-string (str "[" s "]")))

(defn make-env
  [program input]
  {:pos 0
   ;; converting to sorted-map to allow access to memory locations outside the original size
   :data (reduce-kv assoc (sorted-map) program)
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
  [{:keys [pos data] :as env}]
  (let [op->arity {1 3
                   2 3
                   3 1
                   4 1
                   5 2
                   6 2
                   7 3
                   8 3
                   9 1
                   99 0}
        [op & param-modes] (read-instruction (get data pos))
        arity (op->arity op)
        _ (when (nil? arity)
            (throw (Exception. (str "Arity not found for op " op))))
        raw-args (->> (range)
                      (drop (inc pos))
                      (take arity)
                      (mapv #(get data %)))]
    {:op op
     :arity arity
     :raw-args raw-args
     :param-modes (take arity param-modes)}))

(defn get-param-address
  [{:keys [relative-base]} param-mode value]
  (case param-mode
    0 value
    1 value
    2 (+ relative-base value)))

(defn get-param-value
  [{:keys [data] :as env} param-mode value]
  (if (= 1 param-mode)
    value
    (get data (get-param-address env param-mode value) 0)))

(defn eval-expr
  [{:keys [pos data relative-base in out] :as env}
   {:keys [op arity raw-args param-modes] :as expr}]
  ;; (prn `(eval-expr ~env ~expr))
  (let [next-pos (+ pos (inc arity))
        param-values (mapv #(get-param-value env %1 %2) param-modes raw-args)
        param-addresses (mapv #(get-param-address env %1 %2) param-modes raw-args)
        updates (case op
                  ;; add
                  1 (let [[a b] param-values
                          dest (last param-addresses)]
                      {:data (assoc data dest (+ a b))})
                  ;; mult
                  2 (let [[a b] param-values
                          dest (last param-addresses)]
                      {:data (assoc data dest (* a b))})
                  ;; read
                  3 (let [[dest] param-addresses]
                      (assert (not-empty in) "Tried to read from empty input.")
                      {:data (assoc data dest (peek in))
                       :in (pop in)})
                  ;; write
                  4 (let [[value] param-values]
                      {:out (conj out value)})
                  ;; jump-if-true
                  5 (let [[test jump-pos] param-values]
                      (when-not (zero? test)
                        {:pos jump-pos}))
                  ;; jump-if-else
                  6 (let [[test jump-pos] param-values]
                      (when (zero? test)
                        {:pos jump-pos}))
                  ;; less than
                  7 (let [[a b] param-values
                          dest (last param-addresses)]
                      {:data (assoc data dest (if (< a b) 1 0))})
                  ;; equal
                  8 (let [[a b] param-values
                          dest (last param-addresses)]
                      {:data (assoc data dest (if (= a b) 1 0))})
                  ;; adjust relative-base
                  9 (let [[value] param-values]
                      {:relative-base (+ relative-base value)})
                  ;; quit
                  99 {:done true}
                  (throw (Exception. (str "Unrecognized op for expression: " expr))))]
    (merge env {:pos next-pos} updates)))

(defn step
  "Run a single step."
  [env]
  (eval-expr env (read-expr env)))

(defn run
  ([program] (run program []))
  ([program input]
   (loop [env (make-env (if (string? program)
                          (read-syms program)
                          program)
                        input)]
     (if (:done env)
       env
       (recur (step env))))))
