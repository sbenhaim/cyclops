(ns cyclops.events
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.pprint :refer [print-table]]))


(defrecord TimeContext [from to])


(defn tc [from to]
  (map->TimeContext {:from from :to to}))

(defprotocol Value
  (realize [this ^TimeContext ctx]))


(defn value? [e] (satisfies? Value e))


(defrecord BasicValue [v]
  Value
  (realize [_ _] v))


(defrecord Event [value start end period]
  Comparable
  (compareTo [this that]
    (compare [(:start this) (:end this)] [(:start that) (:end that)]))
  Value
  (realize [_ ctx] (realize value ctx)))


(defn length [^Event e]
  (- (:end e) (:start e)))


(defprotocol Cyclic
  (period [this])
  (events [this])
  (slice
    [this ^TimeContext tc]
    [this ^TimeContext tc opts]))


(defn cycle-events
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([cyc] (cycle-events nil cyc))
  ([n cyc]
   (let [period (period cyc)
         evts (events cyc)
         cycle (->> evts
                    repeat
                    (mapcat
                     (fn [i cycle]
                       (for [e cycle]
                         (-> e
                             (update :start #(+ % (* i period)))
                             (update :end #(+ % (* i period))))))
                     (range)))]
     (if n
       (take (* n (count evts)) cycle)
       cycle))))




(defn -slice- [cyc from to {:keys [mode] :or {mode :starts-during}}]
  (assert (#{:starts-during :ends-during :active-during} mode))
  (let [evts (events cyc)
        p    (period cyc)]
    (if (and (zero? from) (= to p)) evts
        (let [to                (if (<= to from) (+ to p) to)
              loop              (if (> to p) (cycle-events cyc) evts)
              [key1 key2 compr] (case mode
                                  :starts-during [:start :start >]
                                  :ends-during   [:end :end >]
                                  :active-during [:end :start >=])
              slc               (into []
                                      (comp
                                       (drop-while #(compr from (key1 %)))
                                       (take-while #(> to (key2 %))))
                                      loop)]
          slc))))



(defn offset-slice [amount slc]
  (map (fn [e]
         (-> e
             (update :start #(+ % amount))
             (update :end #(+ % amount))))
       slc))


(defrecord Cycle [period events]
  Cyclic
  (period [_] period)
  (events [_] events)
  (slice [this tc] (-slice- this (:from tc) (:to tc) {:mode :starts-during}))
  (slice [this tc opts] (-slice- this (:from tc) (:to tc) opts)))



(defn lcp [& cycles]
  (reduce (fn [m c] (lcm m (period c))) (period (first cycles)) (rest cycles)))


(defn normalize-periods
  [a b]
  (let [p (lcp a b)
        a      (cycle-events (/ p (period a)) a)
        b      (cycle-events (/ p (period b)) b)]
    [(->Cycle p a) (->Cycle p b)]))


(defn sync-periods
  [a b]
  (let [[a b] (normalize-periods a b)]
    (->Cycle (:period a) (concat (:events a) (:events b)))))



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
         (map #(apply ->Cycle %))
         (reduce sync-periods))))



(defn sin [& {:keys [min max period] :or {min 0 max 1 period 1}}]
  (fn [_ ^TimeContext {:keys [from] :as tc}]
    (let [TAU (* 2 Math/PI)
          mult (-> max (- min) (/ 2))
          base (-> from (* TAU) (/ period) Math/sin (+ 1) (* mult))
          n (+ base min)]
      n)))


(defn rand
  ([] (rand 0 1))
  ([max] (rand 0 max))
  ([min max]
   (fn []
     (let [cap (- max min)]
       (+ (clojure.core/rand cap) min)))))


(defn irand
  ([max] (irand 0 max))
  ([min max]
   (fn []
     (let [cap (- max min)]
       (+ (rand-int cap) min)))))
