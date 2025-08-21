(ns cyclops.util)

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

  Returns `:variadic` if the function/macro is variadic."
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



(defn divisable? [n divisor]
  (zero? (mod n divisor)))


(comment (divisable? 3 9))
