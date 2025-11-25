(ns cycl.ops2
  (:require
   [cycl.event :as e]
   [cycl.sub-op :as sub]
   [cycl.cycl :as c]
   [cycl.util :as u]))


(declare fit)


(defn ->cycl
  [thing]
  (cond
    (c/cycl? thing)  thing
    (sequential? thing)  (apply fit thing)
    (e/event? thing) [thing]
    :else            [(e/->event thing)]))


(defn fit
  [& pattern]
  (sub/fit-op (map ->cycl pattern)))


(comment
  (fit :a :b :c)
  (fit (fit :a :b) :c)
  (fit (fit :a :b))
  (fit (cyc :a :b))
  (fit :a (fit :b :c))
  (fit :a [:b :c])
  (fit [:a [:b :c]])
  (fit (range 4))
  (apply fit (range 4))
  (fit (list :a :b :c)))


(defn cyc
  [& pattern]
  (sub/cycl-op (map ->cycl pattern)))


(comment
  (cyc :a :b :c)
  (cyc :a (cyc :b :c)))


(comment
  (fit :a (cyc :b :c))
  (cyc :a (fit :b :c))
  (fit (cyc :a :b) (cyc :c (fit :d :e)) :f)
  (cyc (fit :a :b) (cyc :c (fit :d :e)) :f))

(defn ->op*
  [op argpat pattern]
  (let [args (-> argpat u/gimme-vec)
        cycl (fit pattern)]
    (if (= 1 (count args))
      (op (first args) cycl)
      (sub/op-merge op (->cycl args) cycl))))


(defn x
  [n* & pattern]
  (->op* sub/times-op n* pattern))


(comment
  (x 2 :a)
  (x 2 :a :b)
  (x 2 [:a :b])
  (x 2 (fit :a :b))
  (x 2 (range 2))

  (x [2 1] [:a :b])
  (x (cyc 2 1) [:a :b])
  )


(defn el
  [x* & pattern]
  (->op* sub/elongate-op x* pattern))


(comment
  (fit :a (el 2 :b))
  (fit :a (el 2 :b :b))
  (fit :a (el 2 (fit :b :b)))
  (fit :a (el 2 [:b :b]))
  (fit (el 2 :a) :b)
  (fit (el 2 :a) (el 2 :b))
  (fit :a (fit :b (el 2 :c)))
  (cyc :a (el 2 :b))
  (cyc :a (el 2 :b :b))
  (cyc :a (el 2 [:b :b]))
  (cyc :a (el 2 (fit :b :b)))
  (cyc :a (cyc :b (el 2 :c)))
  (el [2 1] :a :b)
  (el (cyc [2 1] 1) :a :b))


(defn rep
  [n* & pattern]
  (->op* sub/repeat-op n* pattern))


(comment
  (fit :a (rep 2 :b))
  (cyc :a (rep 2 :b)))

(defn spl
  [& pattern]
  (let [cycls (map ->cycl pattern)
        weight (reduce + (map sub/weigh cycls))]
    (apply el weight pattern)))


(comment
  (fit :a (spl :b :c))
  (cyc :a (spl :b :c))
  (fit :a (spl :b (el 2 :c)))
  (cyc :a (spl :b (el 2 :c))))


(defn slow
  [x cycl]
  (sub/scale-op x cycl))


(defn speed
  [x cycl]
  (slow (/ x) cycl))



(comment
  (slow 2 (fit :a :b))
  (slow 1/2 (fit :a :b))
  (slow 3/2 (fit :a :b))
  (slow 1/4 (cyc :a (cyc :b :c)))
  (speed 2 (fit :a :b))
  (speed 1/2 (fit :a :b))
  (speed 3/2 (fit :a :b))
  (speed 1/4 (cyc :a (cyc :b :c))))

