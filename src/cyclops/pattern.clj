(ns cyclops.pattern
  (:require
   [cyclops.events :as e]
   [cyclops.music :as m]
   [cyclops.util :as u :refer [cycle-n]]
   [cyclops.merge :as merge]))

;; Ops

(defrecord OpContext
    [period         ;; Number of cycles before looping
     segment-length ;; How much segments does it occupy per cycle
     spacing        ;; How many segments between events
     start          ;; Where it sits in the pattern
     ])


(defprotocol Weighty
  (weigh [this]))


(extend-protocol Weighty
  nil
  (weigh [_] 1)

  java.lang.Object
  (weigh [_] 1))


(defn sum-weights
  "Adds the weights of all children of an op recursively."
  [stuff]
  (reduce + (map weigh stuff)))


(defprotocol Operatic
  (operate [this ctx]))


(def base-context (->OpContext 1 1 1 0))


(defn ->cycl
  "Takes the base of a raw, nested op structure and turns it into a timed cycle."
  ([op] (->cycl op base-context))
  ([op ctx]
   (-> op
       (operate ctx)
       e/normalize)))


;; TODO: This a good idea?
(extend-type cyclops.pattern.Operatic
  e/DoYouRealize?
  (realize [this] (e/realize (->cycl this) nil)))


(defn op?
  [e]
  (satisfies? Operatic e))


;; TODO: Trampoline with operate?
(defn apply-timing
  "Given a collection of events and a timing `Context`, recursively schedules
  the events honoring weights.

  Three behavior modes depending on the pat provided

  1. Op -> Create a nested list of timed events (use `events/normalize` to flatten)
  2. Cycl -> Retimes the events
  3. [maps] -> Treats maps as the params of newly created events

"
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
                       (cond
                         (op? child)      (operate child ctx) ;; If child is an op, apply with inherited context
                         (e/event? child) (assoc child :start start :length length :period period)
                         (map? child)     (e/->Event child start length period)
                         :else            (e/->event child ;; Otherwise, add timing information to the event. Event keys can override (e.g., `length`).
                                                     start
                                                     length
                                                     period))))))))


(defn squeeze
  "Squeeze children into inherited segment. Spacing and segment length will be
  the same. Period remains unchanged."
  [children ^OpContext {:keys [segment-length] :as context}]
  (if-not
      (seq children) []
      (let [n              (sum-weights children)
            segment-length (/ segment-length n)]
        (apply-timing children
                      (assoc context
                             :spacing segment-length
                             :segment-length segment-length)))))


(extend-type clojure.lang.Sequential
  Operatic
  (operate [this ctx] (squeeze this ctx)))


(defrecord FitOp [children]
  Operatic
  (operate [_ ctx] (squeeze children ctx)))


(defrecord SpliceOp [children]
  Operatic
  (operate [_this ctx]
    (squeeze children ctx))
  Weighty
  (weigh [_] (sum-weights children)))


(defn basic-ctrl
  "Ctrl that moves value from `:init` to `:param`"
  [param pat]
  (->> pat ->cycl (map #(e/reassoc-param % :init param))))


(defn op-merge
  "Given a fn that applies an operator to a single arg, a arg Pattern and a
  value pattern, operates on the merge of arguments with values."
  [->op arg-pat val-pat]
  (if (= 1 (count arg-pat))
    (->op (first arg-pat) (->cycl val-pat))
    (let [param    (gensym)
          args     (basic-ctrl param arg-pat)
          cycl     (->cycl val-pat)
          merge-fn (fn [o evts] (->op (get-in o [:params param]) evts))]
      (merge/merge-cycles merge-fn args cycl :op-merge))))


(defrecord TimesOp [n children]
  Operatic
  (operate [_ ctx]
    (squeeze (u/cycle-n n children) ctx)))



(comment (-> (->TimesOp 3 [:a]) ->cycl)
         (-> [(->TimesOp 3 [:a]) :b] ->cycl)
         (-> (->TimesOp 3 [(e/->event :a 0 1 1)]) ->cycl))


(defrecord TimesOp* [n* children]
  Operatic
  (operate [_ ctx]
    (operate (op-merge ->TimesOp n* children) ctx)))


(comment
  (-> (->TimesOp* [2] [:a]) ->cycl)
  (-> (->TimesOp* [2] [:a :b]) ->cycl)
  (-> (->TimesOp* [2 2] [:a :b]) ->cycl)
  (-> (->FitOp [:c (->TimesOp* [2] [:a])]) ->cycl)
  (-> (->FitOp [(->TimesOp* [2] [:a :b]) :c]) ->cycl)
  (-> [(->TimesOp* [2 2] [:a :b]) :c] ->cycl))


(defrecord RepOp [n children]
  Operatic
  (operate [_ ctx]
    (squeeze (cycle-n n children) ctx))
  Weighty
  (weigh [_] (* n (sum-weights children))))


(comment
  (-> (->RepOp 2 [:a :b]) ->cycl)
  (-> (->FitOp [(->RepOp 2 [:a :b]) :c]) ->cycl))


(defn- rep-op-merge
  [n* children]
  (op-merge ->RepOp n* children))


(defrecord RepOp* [n* children]
  Operatic
  (operate [_ ctx]
    (operate (rep-op-merge n* children) ctx))
  Weighty
  (weigh [_] (sum-weights (rep-op-merge n* children))))



(comment
  (-> (->RepOp* [2] [:a]) ->cycl)
  (-> (->RepOp* [2] [:a]) weigh)
  (-> (->RepOp* [2] [:a :b]) ->cycl)
  (-> (->RepOp* [2] [:a :b]) weigh)
  (-> (->RepOp* [2 2] [:a :b]) ->cycl)

  (-> (->FitOp [(->RepOp* [2] [:a]) :c]) ->cycl)
  (-> (->FitOp [(->RepOp* [2] [:a :b]) :c]) ->cycl)
  (-> (->FitOp [(->RepOp* [2 2] [:a :b]) :c]) ->cycl)

  (-> (->SlowOp 3 [:a (->RepOp* [2] [:a])]) ->cycl)
  (-> (->SlowOp 3 [(->RepOp* [2] [:a]) :b]) ->cycl)
  (-> (->SlowOp 5 [(->RepOp* [2] [:a :b]) :c]) ->cycl)
  (-> (->SlowOp 5 [(->RepOp* [2 2] [:a :b]) :c]) ->cycl)


  )


;; Period Ops

(defn stretch
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
  Operatic
  (operate [_ ctx]
    (stretch x children ctx)))


(comment
  (-> (->SlowOp 2 [:a]) ->cycl)
  (-> (->SlowOp 2 [:a :b]) ->cycl)
  (-> (->SlowOp 2 [:a :b :c]) ->cycl)
  (-> (->FitOp [(->SlowOp 2 [:a :b]) :c]) ->cycl)
  )


(defrecord SlowOp* [x* children]
  Operatic
  (operate [_ ctx]
    (operate
     (op-merge ->SlowOp x* children)
     ctx)))


(comment
  (-> (->SlowOp* [2] [:sd]) ->cycl)
  (-> (->SlowOp* [2] [:sd :bd]) ->cycl)
  (-> (->SlowOp* [2 2] [:sd :bd]) ->cycl)
  (-> (->SlowOp* [2 1] [:sd :bd]) ->cycl)

  (-> (->FitOp [:cr (->SlowOp [2] [:sd])]) ->cycl)
  (-> (->FitOp [:cr (->SlowOp [2] [:sd :bd])])   ->cycl)
  (-> (->FitOp [:cr (->SlowOp [2 2] [:sd :bd])]) ->cycl)
  (-> (->FitOp [:cr (->SlowOp [2 1] [:sd :bd])]) ->cycl)


  )


(defrecord CyclOp [children]
  Operatic
  (operate [_ ctx]
    (stretch (sum-weights children) children ctx)))


(comment
  (-> (->CyclOp [:sd])         ->cycl)
  (-> (->CyclOp [:sd :bd])     ->cycl)
  (-> (->CyclOp [:sd :bd :cr]) ->cycl)
  )


(defrecord MaybeOp [x children]
  Operatic
  (operate [_ ctx]
    (squeeze (map (u/p u/maybe x) children) ctx))
  Weighty
  (weigh [_] (sum-weights children)))


(comment
  (-> (->MaybeOp 1/2 [:a :b]) ->cycl (e/realize nil))
  (-> (->MaybeOp 1/2 [[:a :b]]) ->cycl (e/realize nil))
  (-> (->MaybeOp 1/2 [:c [:a :b]]) ->cycl (e/realize nil))
  (-> [:c (->MaybeOp 1/2 [:a :b])] ->cycl (e/realize nil))
  (-> (->CyclOp [(->MaybeOp 1/2 [:b :c]) :a]) ->cycl (e/realize nil))
  (-> [:c (->MaybeOp 1/2 (->SpliceOp [:a :b]))] ->cycl (e/realize nil))
  (-> [:c (->MaybeOp 1/2 [:a :b])] ->cycl (e/realize nil))
  ,)


(defrecord MaybeOp* [x* children mode]
  Operatic
  (operate [_ ctx]
    (letfn [(maybe1 [x kids]
              (->MaybeOp x (cond-> (map e/get-init kids)
                             (= mode :all) vector)))]
      (operate (op-merge maybe1 x* children) ctx)))
  Weighty
  (weigh [_] (sum-weights children)))


(comment
  (-> (->MaybeOp* [0 1] [:a :b] :all) ->cycl (e/realize nil))
  (-> (->MaybeOp* [1/2] [:a :b] :per) ->cycl (e/realize nil))

  (-> (->MaybeOp* [0 1 1/2] [:a :b [:c :d]] :all) ->cycl (e/realize nil))
  (-> (->MaybeOp* [0 1 1/2] [:a :b [:c :d]] :per) ->cycl (e/realize nil))

  (-> (->MaybeOp* [0 1 1] [:a :b (->MaybeOp 1/2 [:c :d])] :per) ->cycl (e/realize nil))

  (-> [(->MaybeOp* [1/2] [(->SpliceOp [:a :b])] :per) :c] ->cycl (e/realize nil))
  (-> [(->MaybeOp* [1/2] [(->SpliceOp [:a :b])] :all) :c] ->cycl (e/realize nil))
  ,)


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


(defrecord EuclidOp [k n r children]
  Operatic
  (operate [_ ctx]
    (let [mask            (bjork (repeat k [true]) (repeat (- n k) [nil]))
          mask            (u/rot mask (or r 0))
          children        (map #(and % children) mask)]
      (squeeze children ctx)))
  Weighty
  (weigh [_]
    (* n (weigh val))))


(defrecord PickOp [children]
  Operatic
  (operate [_ ctx]
    (squeeze [#(rand-nth children)] ctx))
  Weighty
  (weigh [_] (weigh (first children))))


(defrecord ElongateOp [x children]
  Operatic
  (operate [_ ctx]
    (squeeze children ctx))
  Weighty
  (weigh [_] (* x (sum-weights children))))


(comment
  (-> [:a (->ElongateOp 2 [:b])] ->cycl)
  (-> (->CyclOp [:a (->ElongateOp 2 [:b])]) ->cycl))


(defn- op-merge-elong
  [x* children]
  (op-merge ->ElongateOp x* children))


(defrecord ElongateOp* [x* children]
  Operatic
  (operate [_ ctx]
    (operate (op-merge-elong x* children) ctx))
  Weighty
  (weigh [_] (sum-weights (op-merge-elong x* children))))


(comment
  (-> (->ElongateOp* [2] [:a]) weigh)
  (-> (->FitOp [(->ElongateOp* [2] [:a]) :b]) ->cycl))



(defrecord StackOp [children]
  Operatic
  (operate [_ ctx]
    (mapcat
     #(operate % ctx)
     (map vector children)))
  Weighty
  (weigh [_] (apply max (map weigh children))))


;; Controls
;; TODO: Just move to op?
(defn ->ctrl
  [param value-tx pat]
  (->> pat
       ->cycl
       (map (fn [evt] (e/reassoc-param evt :init param #(u/defer (u/collate value-tx) %))))))



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


(comment
 (parse-num [:a :b]))


(defn parse-sound
  [s]
  (cond
    (rest? s)    nil
    (keyword? s) (name s)
    :else        s))
