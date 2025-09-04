(ns cyclops.events
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [cyclops.util :as u :refer [arity ensure-vec]]))


(defn event-sort [a b]
  (compare [(:start a) (:length a)] [(:start b) (:length b)]))


(defrecord Event [params start length period]
  Comparable
  (compareTo [this that]
    (event-sort this that)))


(defn ->event [init start length period]
  (->Event {:init init} start length period))


(defn update-param
  [e k f & vs]
  (apply update-in e [:params k] f vs))


(defn reassoc-param
  ([e from to] (reassoc-param e from to identity))
  ([e from to f]
   (-> e
       (assoc-in [:params to] (f (get-in e [:params from])))
       (update :params #(dissoc % from)))))


(defn qxf
  ([v xf]
   (conj (ensure-vec v) xf))
  ([e param xf]
   (update-param e param #(qxf % xf))))


(defn end [^Event e]
  (+ (:start e) (:length e)))


(defprotocol Cyclic
  (period [this])
  (events [this])
  (cycle-xfs [this])
  (param-xfs [this]))


(defrecord Cycle [period events cycle-xfs param-xfs]
  Cyclic
  (period [_] period)
  (events [_] events)
  (cycle-xfs [_] cycle-xfs)
  (param-xfs [_] param-xfs))


(defn ->cycle [period evts]
  (->Cycle period evts [] {}))


(defn q-cycle-xf
  [cyc xf]
  (update cyc :cycle-xfs #(conj % xf)))


(defn q-event-xf
  [cycl exf]
  (update cycl :cycle-xfs (fn [xfs] (conj xfs #(map exf (events %))))))


(defn q-param-xf
  [cyc param xf]
  (update-in cyc [:param-xfs param] #(conj (or % []) xf)))


(defn map-events
  [f cyc]
  (update cyc :events #(map f %)))


(defn map-params
  [param f cyc]
  (map-events #(update-in % [:param param] f) cyc))




(defn cycle-events
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([cyc] (cycle-events nil cyc))
  ([n cyc]
   (let [period (period cyc)
         evts (events cyc)
         cycle  (->> evts
                     repeat
                     (mapcat
                      (fn [i cyc]
                        (map (fn [e] (update e :start #(+ % (* i period)))) cyc))
                      (range)))]
     (if n
       (take (* n (count evts)) cycle)
       cycle))))


(defn realize-val
  [v ctx]
  (let [param-xfs (get-in ctx [:cycle :param-xfs (:param ctx)] [])
        realized  (cond
                    (coll? v) (reduce (fn [prev cur] (realize-val cur (assoc ctx :prev prev)))
                                      (realize-val (first v) ctx) (rest v))
                    (fn? v)   (if (zero? (arity v))
                                (v)
                                (v ctx))
                    :else     v)]
    (u/reduce-apply realized param-xfs)))


(defn realize-event
  ([^Event e] (realize-event e {}))
  ([^Event e ctx]
   (let [base     (select-keys e #{:start :length})
         params   (:params e)
         ctx      (assoc ctx :event e)
         realized (into {} (for [[k v] params] [k (realize-val v (assoc ctx :param k))]))]
     (merge base realized))))


(defn realize-events [ctx events]
  (map #(realize-event % ctx) events))


(defn realize
  "Realizes Cycle into slice of events.
  ;; TODO: Slice + realize should be transducer thing"
  ([cyc] (realize cyc {}))
  ([cyc ctx]
   (let [ctx (assoc ctx :cycle cyc)
         evts (->> cyc
                   events
                   (map #(realize-event % ctx)))]
     (u/reduce-apply evts (cycle-xfs cyc)))))


(defn slice [cyc from length {:keys [mode] :or {mode :starts-during}}]
  (assert (#{:starts-during :ends-during :active-during} mode))
  (let [p             (period cyc)
        evts         (events cyc)
        to            (+ from length)
        to            (if (<= to from) (+ to p) to)
        loop          (if (> to p) (cycle-events evts) evts)
        [drop? take?] (case mode
                        :starts-during [#(> from (:start %)) #(> to (:start %))]
                        :ends-during   [#(> from (end %)) #(> to (end %))]
                        :active-during [#(>= from (end %)) #(> to (:start %))])
        slc           (into []
                            (comp
                             (drop-while drop?)
                             (take-while take?))
                            loop)]
    slc))



(defn offset [amount slc]
  (map #(update % :start (partial + amount)) slc))


(defn lcp [& cycles]
  (reduce (fn [m c] (lcm m (period c))) (period (first cycles)) (rest cycles)))


(defn normalize-periods
  [a b]
  (let [p (lcp a b)
        a (cycle-events (/ p (period a)) a)
        b (cycle-events (/ p (period b)) b)]
    [(->cycle p a) (->cycle p b)]))


(defn sync-periods
  [a b]
  (let [[a b] (normalize-periods a b)]
    (->cycle (period a) (concat (events a) (events b)))))


(defn collapse-events
  "Takes a potentially nested collection of events of differing periods
  and creates a flat list of events of a single period.
  Returns a tuple of `[period evts]`.

  Note: Does not currently update :period values in the events themselves."
  [evts]
  (if-not (seq evts)
    []
    (->> evts
         flatten
         (group-by :period)
         (map #(apply ->cycle %))
         (reduce sync-periods))))
