(ns cycl.p2
  (:require [cycl.events :as e]
            [cycl.util :as u]
            [cycl.merge :as merge]))




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




(defn normalize
  [cycl]
  (for [evt cycl]
    (let [[whole part] (u/compound-fraction (:start evt))]
      (-> evt
          (assoc :start part)
          (update :iter #(+ % whole))))))


(defn unoffset
  [cycl]
  (let [n (get-in cycl [0 :start])]
    (offset cycl (- n) :start)))


(defn cycl-len
  ([cycl] (cycl-len cycl (apply min (map :iter cycl))))
  ([cycl iter]
   (reduce + (for [evt cycl :when (= iter (:iter evt))]
               (:length evt)))))



(defn cycls-len
  [cycls]
  (let [iter (apply min (map :iter (first cycls)))]
    (reduce + (map #(cycl-len % iter) cycls))))


(defn el-op [x cycl]
  (scale cycl x :start :length))


(defn by-iter
  [cycls]
  (let [iters (distinct (for [cycl cycls evt cycl] (:iter evt)))]
    (into {} (for [iter iters]
               [iter
                (keep (fn [cycl]
                        (seq (filter (fn [e] (= (:iter e) iter)) cycl)))
                      cycls)]))))


(comment
  (fit-op [(vector {:params {:init :a}, :start 0N, :length 1/2, :iter 0, :period 2N}
                   {:params {:init :a}, :start 1/2, :length 1/2, :iter 0, :period 2N})
           (vector {:params {:init :b}, :start 0N, :length 1N, :iter 0, :period 2N})
           (vector {:params {:init :a}, :start 0N, :length 1/2, :iter 1, :period 2N}
                   {:params {:init :b}, :start 1/2, :length 1/2, :iter 1, :period 2N})]))



(defn fit-op
  [cycls]
  (->>
   (for [[_iter cycls] (by-iter cycls)]
     (let [cycle-len    (cycls-len cycls)
           segmentation (/ cycle-len)]
       (if (and false (<= cycle-len 1))    ; TODO:
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
   flatten
   (sort e/event-compare)))


(fit-op [[{:start 0 :length 1/2 :iter 0}]])


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


(defn ->cycl?
  [thing]
  (cond
    (e/cycl? thing)  thing
    (e/event? thing) [thing]
    (map? thing)     [(e/->Event thing 0 1 0 1)]
    :else            [(e/->event thing)]))


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
  (encyclify [[:a :b] '(:c :d)])
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
  (fit-op
   (repeat n cycl)))


(defn op-merge
  "Given a fn that applies an operator to a single arg, a arg Pattern and a
  value pattern, operates on the merge of arguments with values."
  [op1 arg-cycl val-cycl]
  (if (= 1 (count arg-cycl))
    (op1 (-> arg-cycl first e/get-init) val-cycl)
    (let [param (gensym)
          args  (map #(e/reassoc-param % :init param) arg-cycl)
          merge-fn (fn [arg-evt val-evts] (op1 (e/get-param arg-evt param) (unoffset val-evts)))
          evts  (merge/merge-cycles merge-fn args val-cycl :op-merge)]
      evts
      (fit-op evts)
      )))



(comment
  println
  (op-merge times-op (cyc [2 1] 1) (fit :a :b))
  (op-merge el-op (cyc [2 1] [1 1]) (fit :a :b))
  (op-merge rep-op (cyc [2 1] [1 1]) (fit :a :b))
  (op-merge rep-op (cyc 2 1) (fit :a))
  (op-merge times-op (fit 2 2) (fit :a :b))


  (fit-op [(vector {:params {:init :a}, :start 0N, :length 1/2, :iter 0, :period 2N}
                   {:params {:init :a}, :start 1/2, :length 1/2, :iter 0, :period 2N})
           (vector {:params {:init :b}, :start 0N, :length 1N, :iter 0, :period 2N})
           (vector {:params {:init :a}, :start 0N, :length 1/2, :iter 1, :period 2N}
                   {:params {:init :b}, :start 1/2, :length 1/2, :iter 1, :period 2N})])


  (map #(filter (fn [e] (= (:iter e) 1)) %) [(vector {:params {:init :a}, :start 0N, :length 1/2, :iter 0, :period 2N}
                       {:params {:init :a}, :start 1/2, :length 1/2, :iter 0, :period 2N})
               (vector {:params {:init :b}, :start 0N, :length 1N, :iter 0, :period 2N})
               (vector {:params {:init :a}, :start 0N, :length 1/2, :iter 1, :period 2N}
                       {:params {:init :b}, :start 1/2, :length 1/2, :iter 1, :period 2N})])


  )

(defn x
  [n* & pattern]
  (op-merge times-op (first (encyclify [n*])) (apply fit pattern)))


(comment
  (x 2 :a :b)
  (x (fit 2 2) :a :b))


(defn el
  [n & pattern]
  (el-op n (apply fit pattern)))


(defn rep-op
  [n cycl]
  (-> (x n cycl)
      (scale n :start :length)))


(fit-op
 [(rep-op 3 (fit :a))])


(defn rep
  [n* & pattern]
  (op-merge rep-op (first (encyclify [n*])) (apply fit pattern)))


(comment
  (cyc (rep 2 :a) :b))


(fit-op
 [(rep-op 2 (fit :a))])
