(ns cyclops.events
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [cyclops.util :refer [arity]]))


(defrecord TimeContext [from to])


(defn tc [from to]
  (map->TimeContext {:from from :to to}))


(defrecord Event [value start length period]
  Comparable
  (compareTo [this that]
    (compare [(:start this) (:end this)] [(:start that) (:end that)])))


(def timing-keys #{:start :end :period})


(defn evt->tc [^Event e]
  (tc (:start e) (:end e)))


(defn length [^Event e]
  (- (:end e) (:start e)))


(defprotocol Cyclic
  (period [this])
  (events [this realize?])
  (set-events [this evts])
  (update-events [this f])
  (slice [this from to opts]))


(defn umap-events [f cyc]
  (update-events cyc #(map f %)))


(defn cycle-events
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([cyc] (cycle-events nil cyc))
  ([n cyc]
   (let [period (period cyc)
         evts (events cyc false)
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


(defn realize-val [tc v]
  (if (fn? v)
      (case (arity v)
        0 (v)
        1 (v tc)
        v)
      v))


(defn realize [^Event e]
  (let [tc (evt->tc e)
        f (partial realize-val tc)
        timing (select-keys e timing-keys)]
    (-> (apply dissoc e timing-keys)
        (update-vals f)
        (merge timing))))


(defn -slice- [cyc from to {:keys [mode realize?] :or {mode :starts-during realize? false}}]
  (assert (#{:starts-during :ends-during :active-during} mode))
  (let [evts (events cyc false)
        p    (period cyc)]
    (if (and (zero? from) (= to p)) (if realize? (map realize evts) evts)
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
          (if realize?
            (map realize slc)
            slc)))))



(defn offset-slice [amount slc]
  (map (fn [e]
         (-> e
             (update :start #(+ % amount))
             (update :end #(+ % amount))))
       slc))


(defrecord Cycle [p es]
  Cyclic
  (period [_] p)
  (events [_ realize?] (if realize? (map realize es) es))
  (set-events [this evts] (assoc this :es evts))
  (update-events [this f] (update this :es f))
  (slice [this from to opts] (-slice- this from to opts)))



(defn lcp [& cycles]
  (reduce (fn [m c] (lcm m (period c))) (period (first cycles)) (rest cycles)))


(defn normalize-periods
  [a b]
  (let [p (lcp a b)
        a (cycle-events (/ p (period a)) a)
        b (cycle-events (/ p (period b)) b)]
    [(->Cycle p a) (->Cycle p b)]))


(defn sync-periods
  [a b]
  (let [[a b] (normalize-periods a b)]
    (->Cycle (period a) (concat (events a false) (events b false)))))



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



(defn sin
  ([] (sin 0 1 1))
  ([max] (sin 0 max 1))
  ([min max] (sin min max 1))
  ([min max period]
   (fn [^TimeContext {:keys [from]}]
     (let [TAU  (* 2 Math/PI)
           mult (-> max (- min) (/ 2))
           base (-> from (* TAU) (/ period) Math/sin (+ 1) (* mult))
           n    (+ base min)]
       n))))


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
