(ns shhh.ops
  (:require [shhh.util :refer [cycle-n]]))

(def event? (some-fn map? string? keyword?))


(defrecord Context
    [period            ;; Number of cycles before looping
     segment-length    ;; How much space does it take per cycle
     spacing           ;; How much space between events
     position          ;; Where it sits in the pattern
     parent            ;; The containing loop
     ])




(def base-context (->Context 1 1 1 0 []))

(comment (ns-unmap *ns* 'apply-op))

(defmulti apply-op
  "Given an op sub-pattern in the form of `[op & args & events]` and the the
  Context from the parent sub-pattern, returns a vector of `[[timed-events] ^Context c]`
  where `timed-events` are events (minus `op` and `args`) with positions applied
  and c is the next context for the sub-pattern."
  (fn [[op & _args+events] _parent-context] op))


(defn assign-times
  [pattern]
  (apply-op pattern base-context))


(defn space-evenly
  "Given a collection of untimed events and a Context, spaces the events
  out evenly."
  [events {:keys [period segment-length spacing position] :as context}]
  (map-indexed
   (fn [i e]
     (let [pos             (+ position (* i spacing))
           segment-context (assoc context :position pos)]
       (if (event? e)
         (merge {:position pos
                 :period period
                 :duration segment-length} e)
         (apply-op e segment-context))))
   events))


(defmethod apply-op :default [subpat context]
  (apply-op (into [:fit] subpat) context))


(defn slow
  [events x {:keys [period segment-length] :as context}]
  (let [n              (count events)
        cycle-period   (* x period)
        spacing        (/ cycle-period n)
        segment-length (/ (* segment-length x) n)]
    (space-evenly events (assoc context
                                :period cycle-period
                                :spacing spacing
                                :segment-length segment-length))))


(defmethod apply-op :slow
  [[_op x & events] context]
  (slow x events context))


(defn fit
  [events {:keys [segment-length] :as context}]
  (let [n              (count events)
        spacing        (/ segment-length n)
        segment-length spacing]
    (space-evenly events (assoc context
                                :spacing spacing
                                :segment-length segment-length))))

;; Doesn't change period
;; Divides note duration
(defmethod apply-op :fit
  [[_op & events] context]
  (fit events context))

(comment (apply-op [:fit {} {} [{} {}]] base-context))



(defmethod apply-op :cycle
  [[_op & events] context]
  (slow events (count events) context))



(comment (assign-times [:cycle {} [:cycle {} {}]]))


(defmethod apply-op :* ;; Repeat and speed up (doesn't change cycle order)
  [[_op x & events] context]
  (let [events (cycle-n x events)]
    (fit events context)))


(comment
  (apply-op [:* 2 {} {}] base-context))


;; TODO: Requires modifying the parent, which may already be scheduled.
;; Do we need to do depth first expansion?
;; Expand children first, sending down some context, which may come back modified?
;; Or maybe only available in string syntax
(defmethod apply-op :!
  [[_op x & events] context]) ;; Replicate: Repeat and don't speed up (changes cycle order)


(defmethod apply-op :stack
  [[_op & loops] context]
  (->> loops
       (map #(apply-op % context))
       (apply concat))) ;; Polyphony


;; TODO: non-local
(defmethod apply-op :_) ;; Elongate


;; TODO
(defmethod apply-op :_) ;; Euclid


;; TODO: Would either have to be done schedule time, or use a lambda trigger
(defmethod apply-op :|
  [[_op x & events] context] ;; Random choice
  (let [options #(map #(apply-op % context) events)]
    (fn [] (rand-nth options))))


(defmethod apply-op :%) ;; -> cycle (take n)


(defmethod apply-op :? ;; probability
  [[_op prob & events] context])


(comment
  "< a [b c]>"
  [:< :a [:b :c]]

  `(:a (/ [:b :c] 4) ~@(! 8 :bd))
  )
