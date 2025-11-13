(ns cycl.p2
  (:require [cycl.events :as e]
            [cycl.util :as u]))


(defn ->cycl?
  [thing]
  (cond
    (e/cycl? thing)  thing
    (e/event? thing) [thing]
    (map? thing)     [(e/->Event map 0 1 0 1)]
    :else            [(e/->event thing)]))


(defn scale
  [cycl x & keys]
  (for [evt cycl]
    (reduce
     (fn [evt k] (update evt k #(* x %)))
     evt
     keys)))



(defn offset
  [cycl n & keys]
  (for [evt cycl]
    (reduce
     (fn [evt k] (update evt k #(+ n %)))
     evt
     keys)))


(defn cycl-len
  [cycl]
  (reduce + (for [evt cycl :when (zero? (:iter evt))]
              (:length evt))))


(defn cycls-len
  [cycls]
  (reduce + (map cycl-len cycls)))


(defn el-op [x cycl]
  (scale cycl x :start :length))


(defn fit-op
  [cycls]
  (let [cycle-len    (cycls-len cycls)
        segmentation (/ cycle-len)]
    (loop [[cycl & rst] cycls start 0 out []]
      (if (nil? cycl) out
          (let [fitted (-> cycl
                           (scale segmentation :start :length)
                           (offset start :start))
                length (cycl-len fitted)]
            (recur rst
                   (+ start length)
                   (concat out fitted)))))))


(defn cycl-op
  [cycls]
  (let [period (cycls-len cycls)]
    (loop [[cycl & rst] cycls iter 0 out []]
      (if (nil? cycl) out
          (recur rst
                 (+ iter (cycl-len cycl))
                 (concat out (-> cycl
                                 (scale period :iter :period)
                                 (offset iter :iter))))))))


(defn encyclify
  [pattern]
  (reduce
   (fn [cycl thing]
     (cond
       (e/cycl? thing)     (conj cycl thing)
       (e/event? thing)    (conj cycl [thing])
       (map? thing)        (conj cycl [(e/->Event thing 0 1 0 1)])
       (vector? thing)     (conj cycl (fit-op (mapcat #(encyclify [%]) thing)))
       (sequential? thing) (vec (concat cycl (mapcat #(encyclify [%]) thing)))
       :else               (conj cycl [(e/->event thing)])))
   []
   pattern))


(comment
  (encyclify [:a])
  (encyclify [:a :b])
  (encyclify [:a (fit :a :b)])
  (encyclify [:a :b '(:c :d)])
  )


(defn fit
  [& pattern]
  (fit-op (encyclify pattern)))


(comment
  (fit #{:a :b} :b)
  (fit [:a :b])
  (fit '(:a :b)))


(defn cyc
  [& pattern]
  (cycl-op (encyclify pattern)))


(comment
  (cyc (repeat 2 :a))
  (cyc (list :a :b) :c)
  (cyc [:a :b] (cyc :c :d))
  (cyc (vector (range 10)))
  (cyc (fit [:a :b :c]) :c)
  (cyc [:a :b])
  (cyc '(:a :b))
  )


(defn x
  [n & pattern]
  (fit-op (encyclify (u/cycle-n n pattern))))


(defn el
  [n & pattern]
  (el-op n (apply fit pattern)))

(comment
  (fit :a :b)
  (fit :a (el 2 :b))
  (cyc :a :b)
  (cyc :a (el 2 :b))
  (fit (el 2 :a) (cyc :b (el 2 :c)))
  (fit :a (el 2 (cyc :b :c))))


(defn rep
  [n & pattern]
  (apply repeat n pattern))


(comment
  (fit
   [:a :a]
   ;; (x 2 :a) 
   ;; (rep 2 :a)
   ;; '(:a :a)
   :b))


;; Rest of the ops
;; Pattern args
