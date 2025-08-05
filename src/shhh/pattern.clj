(ns shhh.pattern
  (:require [shhh.util :refer [cycle-n]]))

(defn shorthand?
  "Identifies events represented with shorthand notation."
  [e]
  ((some-fn keyword? string? number? fn?) e))


(declare fit)

(defn ->event
  "Expands shorthand notation to an (untimed) event map."
  [sh]
  (cond
    (sequential? sh) (apply fit sh)
    (number? sh) {:n (float sh)}
    (keyword? sh) {:s (name sh)}
    (string? sh) {:s sh}
    (fn? sh) {:fn sh}
    :else sh))


(defn ->events
  "See `shhh.patter/->event`"
  [shs]
  (map ->event shs))


;; Pattern API: Functions used in pattern definition

(defn fit
  "Squeezes all of its events into the enclosing segment."
  [& children]
  {:op :fit
   :children (->events children)})

(comment
  (fit :a :b :c))


(defn times
  "Squeezes all of its events into the enclosing segment."
  [x & children]
  {:op :fit
   :children (->events (cycle-n x children))})


(defn cycle
  "Plays one event per loop."
  [& children]
  {:op :cycle
   :children (->events children)})

(defn slow
  "Stretches events across `x` loops."
  [x & children]
  {:op :slow
   :factor x
   :children (->events children)})


(defn splice
  "Splices events into parent context adjusting segmentation."
  [& children]
  {:op :splice
   :children (->events children)})


(defn rep [x & children]
  {:op :splice
   :children (->events (cycle-n x children))})


#_(defn rep
  "Repeats event `x` times via splice, adjusting segmentation."
  [x & children]
  {:op :rep
   :factor x
   :children (->events children)})

(defn elongate
  "Causes events to take up `x` times as much time, adjusting segmentation."
  [x & children]
  {:op :elongate
   :factor x
   :children (->events children)})



(defn prob
  "Plays event with probability `x` (0 to 1). Plays rest otherwise.
  If applied to group, prob is applied to *each* event, not to entire group."
  [x & children]
  {:op :splice
   :prob x
   :children
   (->events (map (fn [e] (fn [& _] (if (< (rand) x) e "~"))) children))})


(defn euclid [args & children]
  {:op :euclid
   :args args
   :children (->events children)})


(defn stack
  "Takes and number of loops and plays them simultaneously."
  [& children]
  {:op :stack
   :children (->events children)})


(defn rnd
  "Each loop, randomly chooses one of its children."
  [& children]
  {:op :rand-nth
   :children (->events [#(rand-nth children)])})


(defrecord Context
    [period            ;; Number of cycles before looping
     segment-length    ;; How much segments does it occupy per cycle
     spacing           ;; How many segments between events
     position          ;; Where it sits in the pattern
     ])


(def base-context (->Context 1 1 1 0))

(comment (ns-unmap *ns* 'apply-op))

(defmulti apply-op (fn [op _context] (:op op)))


(defn process-pattern
  [pat-root]
  (apply-op pat-root base-context))


(defn op? [e] (:op e))
(defn event? [e] (or (:s e) (:n e)))

(comment (ns-unmap *ns* 'weight))
(defmulti weigh "Determine segment weight (number of cycle segments occupied) of an op. Usually 1." :op)

(defmethod weigh :default
  [_] 1)


(defn weigh-children
  "Adds the weights of all children of an op recursively."
  [children]
  (reduce + (map weigh children)))


(defn apply-timing
  "Given a collection of events and a timing `Context`, recursively schedules
  the events honoring weights."
  [events ^Context {:keys [period spacing segment-length position] :as context}]
  (let [n-segments (weigh-children events)]
    (loop [raw events pos position timed []]
      (if-not
          (seq raw) timed
          (let [[evt & raw] raw
                evt-weight (weigh evt) ;; How many segments to occupy
                evt-length (* evt-weight segment-length)
                op-ctx (assoc context
                              :position pos
                              :segment-length evt-length) ;; Context for children operations
                next-pos (+ pos (* evt-weight spacing))]
            (recur raw next-pos
                   (conj timed
                         (if (op? evt)
                           (apply-op evt op-ctx) ;; If child is an op, apply with inherited context
                           ;; Otherwise, add timing information to the event. Event keys can override (e.g., `length`).
                           (merge {:position pos :period period :length evt-length} evt)))))))))


(defn fit-children
  "Squeeze children into inherited segment. Spacing and segment length will be the same. Period remains unchanged."
  [children {:keys [segment-length] :as context}]
  (let [n              (weigh-children children)
        spacing        (/ segment-length n)
        segment-length spacing]
    (apply-timing children (assoc context
                                  :spacing spacing
                                  :segment-length segment-length))))


(defmethod apply-op :default
  [op context]
  (fit-children (->events op) context))


(defmethod apply-op :fit
  [{:keys [children]} context]
  (fit-children children context))



(defn apply-slow
  "Stretch the children across `factor` segments by altering `period` and stretching `spacing` and `segment-length`."
  [children factor {:keys [period segment-length] :as context}]
  (let [n              (weigh-children children)
        cycle-period   (* factor period)
        spacing        (/ cycle-period n)
        segment-length (/ (* segment-length factor) n)]
    (apply-timing children (assoc context
                                  :period cycle-period
                                  :spacing spacing
                                  :segment-length segment-length))))


(defmethod apply-op :slow
  [{:keys [factor children]} context]
  (apply-slow children factor context))


(defmethod apply-op :cycle
  [{:keys [children]} context]
  (apply-slow children (weigh-children children) context))


(defmethod weigh :splice
  [{:keys [children]}]
  (weigh-children children))


(defn apply-splice
  [children {:keys [segment-length] :as context}]
  (let [n              (weigh-children children)
        segment-length (/ segment-length n)]
    (apply-timing children (assoc context
                                  :spacing segment-length
                                  :segment-length segment-length))))

(defmethod apply-op :splice
  [{:keys [children]} context]
  (apply-splice children context))


#_(defmethod apply-op :rep
  [{:keys [factor children]} context]
  (apply-op (apply-splice (cycle-n factor children) context)))


(defmethod weigh :elongate
  [{:keys [factor children]}]
  (* factor (weigh-children children)))


(defmethod apply-op :elongate
  [{:keys [children]} {:keys [segment-length] :as context}]
  (let [n (weigh-children children)
        segment-length (/ segment-length n)
        spacing segment-length]
    (apply-timing children (assoc context :segment-length segment-length :spacing spacing))))


(defmethod apply-op :stack
  [{:keys [children]} context]
  (mapcat #(apply-op % context) children))



(defmethod apply-op :eucid
  [args & children]
  "todo")
