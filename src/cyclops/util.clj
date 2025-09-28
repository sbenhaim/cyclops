(ns cyclops.util
  (:require
   [clojure.math.combinatorics :as combo]))

(defn cycle-n
  [n seq]
  (let [len (count seq)
        n* (* n len)]
    (take n* (cycle seq))))


(defn toggle! [a]
  (swap! a not))


(defn rot [s n]
  (take (count s) (drop n (cycle s))))


(defn arity
  "Returns the maximum arity of:
    - anonymous functions like `#()` and `(fn [])`.
    - defined functions like `map` or `+`.
    - macros, by passing a var like `#'->`.

  Returns `:variadic` if the function/macro is variadic.

  TODO: SO credit."
  [f]
  (let [func (if (var? f) @f f)
        methods (->> func class .getDeclaredMethods
                     (map #(vector (.getName %)
                                   (count (.getParameterTypes %)))))
        var-args? (some #(-> % first #{"getRequiredArity"})
                        methods)]
    (if var-args?
      :variadic
      (let [max-arity (->> methods
                           (filter (comp #{"invoke"} first))
                           (sort-by second)
                           last
                           second)]
        (if (and (var? f) (-> f meta :macro))
          (- max-arity 2) ;; substract implicit &form and &env arguments
          max-arity)))))


(comment (arity (partial inc 1)))


(defn fn0? [f?]
  (zero? (arity f?)))


(defn fn1? [f?]
  (= 1 (arity f?)))


(defn fn2? [f?]
  (= 2 (arity f?)))


(defn fnv? [f?]
  (= :variadic (arity f?)))



(defn divisable? [n divisor]
  (zero? (mod n divisor)))


(defn reduplicate
  "Convert a map with collections for some of the keys into a list of maps with scalar keys.
  NOTE: List grows combinatorily."
  [m]
  (let [coll-keys (filter #(coll? (m %)) (keys m))
        fixed-keys (remove #(coll? (m %)) (keys m))
        coll-values (map m coll-keys)]
    (map (fn [vals]
           (merge (zipmap coll-keys vals)
                  (select-keys m fixed-keys)))
         (apply combo/cartesian-product coll-values))))


(defn smart-splat
  "Unwraps a seq with only one entry to ~~simplify~~ ease-ify the Cyclops UX."
  [col]
  (if (and (sequential? (first col)) (= 1 (count col)))
    (first col)
    col))



(defn p
  "Partial, but works with the `arity` fn for arities up to 2."
  [f a]
  (case (arity f)
    0 (throw (RuntimeException. "Partial called on argless fn."))
    1 #(f a)
    2 #(f a %)
    (partial f a)))


(defn p2
  [f a]
  #((p f %) a))


(defn cmp
  "Compose, but only accepts 2 fns and works with the `arity` fn for arities up to 2."
  [f1 f2]
  (case (arity f2)
    0 #(f1 (f2))
    1 #(f1 (f2 %))
    2 #(f1 (f2 %1 %2))
    (comp f1 f2)))


(defn defer
  [f arg-or-fn]
  (if (fn? arg-or-fn)
    (cmp f arg-or-fn)
    (p f arg-or-fn)))


(defn gimme-coll
  "If given a scalar, wrap it in the supplied collection. If given a seq, convert it to the supplied collection."
  [col v]
  (if (sequential? v) (into col v)
      (conj col v)))


(defn gimme-vec
  "If given a scalar, return a vector containing that scalar. If given a seq, convert it to a vector."
  [v]
  (gimme-coll [] v))


(defn vector*
  "Throw stuff at it and always get a flat vector back."
  [& stuff]
  (->> stuff vec flatten (into [])))


(comment
  (vector* :a)
  (vector* :a [:b] '(:c :d [:e :f])))


(defn num-enough? [& ns?]
  (every? (some-fn number? nil?) ns?))


(defn reassoc
  ([m from to] (reassoc m from to identity))
  ([m from to f]
   (-> m (assoc to (f (from m))) (dissoc from))))


(defn reassoc?
  ([m from to] (reassoc? m from to identity))
  ([m from to f]
   (if (contains? m from)
     (reassoc m from to f)
     m)))


(defn reduce-apply [v xfs]
  (reduce (fn [v xf] (xf v)) v xfs))


(defn collate [f]
  (fn [v]
    (cond
      (vector? v) (mapv f v)
      (sequential? v) (map f v)
      :else (f v))))


(defn maybe [p v]
  (when (< (rand) p) v))


(defn maybe->
  [v p f]
  (if (p v) (f v) v))


(comment
  (maybe-> 2 zero? inc))
