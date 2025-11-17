(ns cycl.p2
  (:require [cycl.events :as e]
            [cycl.util :as u]
            [cycl.merge :as merge]))


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
  [cycl n & keys]  (for [evt cycl]
    (reduce
     (fn [evt k] (update evt k #(+ n %)))
     evt
     keys)))


(defn cycl-len
  [cycl]
  (let [iter (apply min (map :iter cycl))]
    (reduce + (for [evt cycl :when (= iter (:iter evt))]
                (:length evt)))))



(defn cycls-len
  [cycls]
  (reduce + (map cycl-len cycls)))


(defn el-op [x cycl]
  (scale cycl x :start :length))


(defn fit-op
  [cycls]
  (let [cycle-len    (cycls-len cycls)
        segmentation (/ cycle-len)]
    (if (and false (<= cycle-len 1)) ; TODO:
      (flatten cycls)
      (loop [[cycl & rst] cycls start 0 out []]
        (if (nil? cycl) out
            (let [fitted (-> cycl
                             (scale segmentation :start :length)
                             (offset start :start))
                  length (cycl-len fitted)]
              (recur rst
                     (+ start length)
                     (concat out fitted))))))))


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


(defn times-op
  [n cycl]
  (fit-op (u/cycle-n n pattern)))


(comment
  (fit-op
   [(e/cycle-events 2 (fit :a))]))


(ns-unmap *ns* 'op-merge)
(defn op-merge
  "Given a fn that applies an operator to a single arg, a arg Pattern and a
  value pattern, operates on the merge of arguments with values."
  [op1 arg-cycl val-cycl]
  (if (= 1 (count arg-cycl))
    (op1 (-> arg-cycl first e/get-init) val-cycl)
    (let [param (gensym)
          args  (map #(e/reassoc-param % :init param) arg-cycl)
          ;; merge-fn (fn [a bs] (list op1 (e/get-param a param) bs))
          ;; evts  (merge/merge-cycles merge-fn args val-cycl :op-merge)
          merge-fn (fn [a bs] (op1 (e/get-param a param) bs))
          evts  (merge/merge-cycles merge-fn args val-cycl :op-merge)
          ]
      (fit-op evts)
      ;; (flatten evts)
      #_(for [evt evts]
        (let [arg (e/get-param evt param)]
          `(~op1 ~arg [~(e/dissoc-param evt param)]))))))



(op-merge x1 (fit 2 2) (fit :a :b))

(fit )

(fit-op [[{:start 1/2 :length 1/2 :iter 0 :period 1}]])


(cyc
 (fit :a) (cyc :b :c) (fit :d))


(cycl-op
 [(x1 2 [{:params {:init :a}, :start 0, :length 1/2, :iter 0, :period 1}])
  (x1 1 {:params {:init :a}, :start 1/2, :length 1/2, :iter 0, :period 1})])


(x1 2 (fit :a))



(fit
 (x1 2 [:a])
 (x1 2 [:b]))


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
;; Realization, merging, normalization, timing
