(ns frp
  (:require
   [cyclops.util :refer [arity]]
   [knitty.core :refer [yank]]
   [knitty.deferred :as kd]))


(extend-type clojure.lang.IFn
  clojure.lang.IDeref
  (deref [this] (this)))

(deftype Thing [f]
  clojure.lang.IDeref
  (deref [_] (f)))


(let [a (future (rand-int 10))
      b (future (/ @a 2))]
  [@b @b])
