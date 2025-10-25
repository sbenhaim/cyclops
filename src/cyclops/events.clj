(ns cyclops.events
  (:require
   [cyclops.util :as u :refer [arity p2 lcm]]
   [cyclops.events :as e]))


(defprotocol DoYouRealize?
  (realize [this ctx]))


(extend-protocol DoYouRealize?

  nil
  (realize [_ _] nil)

  java.lang.Object
  (realize [this _ctx] this)

  clojure.lang.IFn
  (realize [this ctx]
    (case (arity this)
      0 (realize (this) ctx)
      1 (realize (this nil) ctx)
      2 (realize (this nil ctx) ctx)
      this)) ; Or (defer this ctx)?

  clojure.lang.ISeq
  (realize [this ctx] (map (p2 realize ctx) this)))


(defrecord Event [params start length period]
  Comparable
  (compareTo [this that]
    (compare [(:start this) (:length this)] [(:start that) (:length that)]))
  DoYouRealize?
  (realize [this ctx]
    (let [realized (into {} (for [[k v] params] [k (realize v (assoc ctx :param k :event this))]))]
      (assoc this :params realized))))


(comment
  (realize (->event #(rand) 0 1/2 1) nil)
  (event?
   (realize (->Event {:init #(rand) :else 5 :ctx identity} 0 1/2 1) nil)))

(defn event? [evt?]
  ((every-pred :period :start :length) evt?))


(comment
  (event? (->Event {:init :hi} 0 1 1))
  (event? (->event :hi 0 1 1))
  (event? {:period 1 :start 1 :length 1})
  (event? {:period 1 :start 1}))


(defn defer+realize
  [f & args]
  (fn [ctx]
    (apply f (map (p2 realize ctx) args))))


(defn ->event [init start length period]
  (->Event {:init init} start length period))


(defn get-param
  [e param]
  (get-in e [:params param]))


(defn get-init
  [e]
  (get-param e :init))


(defn update-param
  [e k f & vs]
  (apply update-in e [:params k] f vs))


(defn reassoc-param
  ([e from to] (reassoc-param e from to identity))
  ([e from to f]
   (-> e
       (assoc-in [:params to] (f (get-in e [:params from])))
       (update :params #(dissoc % from)))))


(defn dissoc-param
  [e param]
  (update e :params #(dissoc % param)))


(defn end [^Event e]
  (+ (:start e) (:length e)))


(defn event-xf
  ([e f] (event-xf e f #{:start :length}))
  ([e f affected]
   (reduce (fn [e k] (update e k f)) e affected)))


(defn map-params
  [param f evts]
  (map #(update-in % [:param param] f) evts))


(defn cycl?
  [v]
  (and (coll? v)
       (every? event? v)))

(comment
  (cycl? [{:start 0 :period 1 :length 5}])
  (cycl? [{:start 0 :period 1}]))


(defn period
  [evts]
  (assert (cycl? evts) "Period requires a cycl, i.e., coll of cyclops.event.Events.")
  (apply max (map :period evts)))


(defn cycle-events
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([evts] (cycle-events nil evts))
  ([n evts]
   (let [period (period evts)
         cycl   (->> evts
                    repeat
                    (mapcat
                     (fn [i cycl]
                       (map (fn [e] (update e :start #(+ % (* i period)))) cycl))
                     (range)))]
     (if n
       (->> cycl
            (map #(assoc % :period (* period n)))
            (take (* n (count evts))))
       cycl))))


(defn slice [evts from length mode]
  (assert (#{:starts-during :ends-during :active-during} mode))
  (let [p             (period evts)
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



(defn offset [amount evts]
  (map #(update % :start (partial + amount)) evts))


(defn lcp [cycls]
  (reduce lcm (map period cycls)))


(comment (lcp [[(->event :a 0 1 2)] [(->event :b 0 1 3) (->event :c 1 1 3)] [(->event :d 2 1 4)]]))


(defn normalize-periods
  [cycls]
  (let [p (lcp cycls)]
    (map #(cycle-events (/ p (period %)) %) cycls)))


(comment
  (normalize-periods [[(->event :a 0 1 2)] [(->event :b 0 1 3) (->event :c 1 1 3)] [(->event :d 2 1 4)]]))


(defn interleave-cycles
  [cycls]
  (->> (normalize-periods cycls)
       (apply concat)
       sort))


(comment
  (interleave-cycles [[(->event :a 0 1 2)] [(->event :b 0 1 3) (->event :c 1 1 3)] [(->event :d 2 1 4)]]))


(defn normalize
  "Takes a potentially nested collection of events of differing periods
  and creates a flat list of events of a single period."
  [evts]
  (if-not (seq evts)
    []
    (->> evts
         flatten
         (map vector)
         interleave-cycles)))

(comment
  (normalize
   [(->event :a 0 1 1) [(->event :b 1/2 1 2) (->event :c 3/2 1 2)] [(->event :d 3/4 1 1)]]))


(comment
  (let [a #(rand-int 10)]
    [(realize a nil) (realize a nil)]))
