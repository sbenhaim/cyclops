(ns cyclops.experimental
  (:require [cyclops.pattern :as p :refer [Operatic]]))


(defmacro defop
  "TODO: Fix."
  {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
  [op-name doc args & methods]
  (let [op-kw     (keyword op-name)
        operate   (first (filter #(= 'operate (first %)) methods))
        weigh     (first (filter #(= 'weigh (first %)) methods))
        op-impl   (first (filter #(= op-name (first %)) methods))
        op-args   (vec (remove #{'&} (butlast args)))
        splatted? (some #{'&} args)
        pat-arg   (if splatted? `(u/smart-splat ~(last args)) (last args))]
    (assert operate "`operate`` must be defined for op.")
    `(let []
       (defmethod operate [Op ~op-kw]
         ~@(rest operate))
       ~@(when weigh
           `(defmethod weigh [Op ~op-kw]
              ~@(rest weigh)))
       ~(if op-impl
          `(defn ~op-name ~doc ~@(rest op-impl))
          `(defn ~op-name ~doc [~@args]
             (->Op ~op-kw ~op-args ~pat-arg))))))

(comment
  (defop Rep [n children]
    :opfn squeeze
    :kidxf #(cycle-n n children)
    :weight #(* n (sum-weights children))))


(extend-type clojure.lang.Sequential
  Operatic
  (operate [this ctx] (p/apply-fit this ctx)))
