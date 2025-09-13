(ns user
  (:require [com.rpl.specter :refer :all]))


(let [c (range 100)]
  (sp/select sp/ALL c))


(select ALL {:a :b :c {:d :e}})

(time
 (setval BEFORE-ELEM :fit [:a :b :c]))

(time
 (into [:fit] [:a :b :c]))

(time
 (concat [:fit] [:a :b :c]))


(time
 (setval BEGINNING [:fit] [:a :b :c]))

(select-any [ALL even?] (range 10))

(select [even?] (range 10))


(splicing-transform
 [ALL (walker vector?) AFTER-ELEM]
 (fn [_] [:a])
 [:fit [:cycle :a :b] :c :d])

(setval [ALL vector? srange] [:a] [:fit [:cycle :a :b] :c :d])


(defn splice-update-if [pred? afn coll]
  (reduce (fn [is i]
            (if (pred? i)
              (into is (afn i))
              (conj is i)))
          []
          coll))


(splice-update-if vector? rest [:fit [:cycle :a :b] :c :d])


(ns-unmap *ns* 'scratch)
(defmulti scratch type)

(defmethod scratch :default
  [x] (type x))


(defmethod scratch cyclops.events.Cyclic
  [x] 'Cyclic)


(defmethod scratch clojure.lang.Sequential
  [x] "seq")

(defmethod scratch clojure.lang.IPersistentVector
  [x] "vec")

(defmethod scratch clojure.lang.PersistentVector
  [x] "vector")
