(ns cyclops.pattern
  (:require
   [cyclops.events :as e]
   [cyclops.music :as m]
   [cyclops.util :refer [rot cycle-n]]
   [clojure.string :as s]
   [cyclops.pattern :as pat]))

;; Ops

(defprotocol Op
  (weight [this])
  (operate [this ^Context context]))


(defrecord OpContext
    [period         ;; Number of cycles before looping
     segment-length ;; How much segments does it occupy per cycle
     spacing        ;; How many segments between events
     start          ;; Where it sits in the pattern
     ])


(defn op? [e] (satisfies? Op e))


(defn weigh [e]
  (if (op? e) (weight e) 1))


(def base-context (->OpContext 1 1 1 0))


(defn process-pattern
  [pat]
  (-> (operate pat base-context)
      e/collapse-events))

(comment (process-pattern [:a :b :c]))


(defn apply-timing
  "Given a collection of events and a timing `Context`, recursively schedules
  the events honoring weights."
  [pat ^OpContext {:keys [period spacing segment-length start] :as context}]
  (loop [pat pat start start events []]
    (if-not
        (seq pat) events
        (let [[child & pat] pat
              weight        (weigh child) ;; How many segments to occupy
              length        (* weight segment-length)
              ctx           (assoc context
                                   :start start
                                   :segment-length length) ;; Context for children operations
              next-start    (+ start (* weight spacing))]
          (recur pat next-start
                 (conj events
                       (if (op? child)
                         (operate child ctx) ;; If child is an op, apply with inherited context
                         ;; Otherwise, add timing information to the event. Event keys can override (e.g., `length`).
                         (e/->Event child
                                    start
                                    (+ start length)
                                    period))))))))


(defn sum-weights
  "Adds the weights of all children of an op recursively."
  [stuff]
  (reduce + (map weigh stuff)))


(defmacro defop
  {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
  [op-name doc args & methods]
  (let [operate (first (filter #(= 'operate (first %)) methods))
        weight   (first (filter #(= 'weight (first %)) methods))
        weight   (or weight '(weight [_] 1))
        record-name (symbol (s/capitalize op-name))]
    (assert operate "`operate must be defined for op.")
    `(let []
       (defrecord ~record-name [~@args]
         Op
         ~weight
         ~operate)
       (defn ~op-name
         ~doc
         [~@args]
         (~(symbol (str "->" record-name)) ~@args))
       (defn ~(symbol (str op-name "*")) ~doc [~@(butlast args) & ~(last args)]
         (~op-name ~@args)))))



;; Fitting Ops

(defn fit-children
  "Squeeze children into inherited segment. Spacing and segment length will be the same. Period remains unchanged."
  [children ^OpContext {:keys [segment-length] :as context}]
  (if-not
      (seq children) []
      (let [n              (sum-weights children)
            spacing        (/ segment-length n)
            segment-length spacing]
        (apply-timing children
                      (assoc context
                             :spacing spacing
                             :segment-length segment-length)))))


;; Any sequence treated like Tidal's `fastcat`, squeezing notes into the containing context.
(extend-type clojure.lang.Sequential
  Op
  (weight [_] 1)
  (operate [this ^OpContext context] (fit-children this context)))


(defrecord TimesOp [n children]
  Op
  (weight [_] 1)
  (operate [_ ctx]
           (let [children (cycle-n n children)]
             (fit-children children ctx))))


(defn splice
  [children ^OpContext {:keys [segment-length] :as context}]
  (let [n              (sum-weights children)
        segment-length (/ segment-length n)]
    (fit-children children (assoc context
                                  :spacing segment-length
                                  :segment-length segment-length))))


(defrecord SpliceOp [children]
  Op
  (weight [_] (sum-weights children))
  (operate [_ ctx] (splice children ctx)))


(defrecord RepeatOp [x children]
  Op
  (weight [_] (* x (sum-weights children)))
  (operate [_ ctx] (splice (cycle-n x children) ctx)))


;; Period Ops

(defn apply-slow
  "Stretch the children across `factor` segments by altering `period` and stretching `spacing` and `segment-length`."
  [x children {:keys [period segment-length] :as context}]
  (let [n              (sum-weights children)
        cycle-period   (* x period)
        spacing        (/ cycle-period n)
        segment-length (/ (* segment-length x) n)]
    (apply-timing children (assoc context
                                  :period cycle-period
                                  :spacing spacing
                                  :segment-length segment-length))))


(defrecord SlowOp [x children]
  Op
  (weight [_] 1)
  (operate [_ ctx] (apply-slow x children ctx)))


(defrecord CycleOp [children]
  Op
  (weight [_] 1)
  (operate [_ ctx] (apply-slow (sum-weights children) children ctx)))


(defrecord MaybeOp [x children]
  Op
  (weight [_] (sum-weights children))
  (operate [_ ctx] (splice (map (fn [e] #(when (< (rand) x) e)) children) ctx)))


(defn bjork
  ([ps os] (bjork ps os []))
  ([ps os res]
   (if (or (not (seq ps)) (not (seq os)))
     (let [step    (concat res ps os)
           [ps os] (split-with #(= (first step) %) step)]
       (if (<= (count os) 1)
         (flatten (concat ps os))
         (recur ps os [])))
     (recur (rest ps) (rest os)
            (conj res (concat (first ps) (first os)))))))


(defrecord EuclidianOp [k n r val]
  Op
  (weight [_] (* n (weigh val)))
  (operate [_ ctx]
    (let [mask     (bjork (repeat k [true]) (repeat (- n k) [nil]))
          mask     (rot mask (or r 0))
          children (map #(and % val) mask)]
      (splice children ctx))))


(defrecord PickOp [children]
  Op
  (weight [_] (weigh (first children)))
  (operate [_ ctx] (splice [#(rand-nth children)] ctx)))


(defrecord ElongateOp
  [n children]
  Op
  (weight [_] (* n (sum-weights children)))
  (operate [_ {:keys [segment-length] :as context}]
           (let [n              (sum-weights children)
                 segment-length (/ segment-length n)
                 spacing        segment-length]
             (apply-timing children (assoc context :segment-length segment-length :spacing spacing)))))


(defrecord StackOp [children]
  Op
  (weight [_] (max (sum-weights children)))
  (operate [_ ctx] (mapcat #(operate % ctx) (map vector children))))


;; Controls

(defrecord Control [cycle key value-tx]
  e/Cyclic
  (period [_] (e/period cycle))
  (events [this] (e/slice this 0 (e/period this) {:realize? true}))
  (slice [this from to] (e/slice this from to {}))
  (slice [_ from to opts]
    (let [evts (e/slice cycle from to opts)]
      (map (fn [e]
             (-> e
                 (assoc key (value-tx (:value e)))
                 (dissoc :value)))
           evts))))


(defmacro defcontrol
    {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
    ([control doc value-tx]
     `(defcontrol ~control ~doc ~(keyword control) ~value-tx))
    ([control doc param value-tx]
     `(do
        (defn ~control ~doc [pat#]
          (->Control (pat/process-pattern pat#) ~param ~value-tx))
        (defn ~(symbol (str control "*")) ~doc [& pat#]
          (~control pat#)))))


(defn ->control
  [param value-tx pat]
  (->Control (pat/process-pattern pat) param value-tx))


(defn rest? [v]
  (or (nil? v) (#{:- "~"} v)))


(defn parse-num
  [n]
  (cond
    (rest? n)    nil
    (keyword? n) (m/note n)
    (string? n)  (m/note n)
    (number? n)  (float n)
    :else        n))


(defn parse-sound
  [s]
  (cond
    (rest? s)    nil
    (keyword? s) (name s)
    :else        s))
