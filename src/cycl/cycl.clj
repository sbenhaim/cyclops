(ns cycl.cycl
  (:require
   [cycl.event :as e]
   [cycl.util :as u :refer [lcm]]))


(defn sort-cycl
  [cycl]
  (sort e/event-compare cycl))


(defn cycl?
  [v]
  (and (coll? v)
       (every? e/event? v)))

(comment
  (cycl? [{:start 0 :period 1 :length 5}])
  (cycl? [{:start 0 :period 1}]))


(defn period
  [cycl]
  #_(apply max (map e/period cycl))
  (-> cycl first e/period))


(defn loop-cycl
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([cycl] (loop-cycl nil cycl))
  ([n cycl]
   (let [period (period cycl)
         loop   (->> cycl
                     repeat
                     (mapcat
                      (fn [i cycl]
                        (map (fn [e] (update e :start #(+ % (* i period)))) cycl))
                      (range)))]
     (if n
       (->> loop
            (map #(assoc % :period (* period n)))
            (take (* n (count cycl))))
       loop))))


(comment
  (loop-cycl 2 [{:start 0 :period 1}])
  (loop-cycl 2 [{:start 1 :period 2}])
  (loop-cycl 2 [{:start 5/2 :period 4}])
  (slice (loop-cycl [{:start 0 :period 1} {:start 1/2 :period 1}])
         0 2 :starts-during)
  (take 2 (loop-cycl [{:start 0 :period 1}])))


(defn slice [from length mode cycl]
  (assert (#{:starts-during :ends-during :active-during} mode))
  (let [p             (period cycl)
        to            (+ from length)
        to            (if (<= to from) (+ to p) to)
        loop          (if (> to p) (loop-cycl cycl) cycl)
        [drop? take?] (case mode
                        :starts-during [#(> from (e/start %)) #(> to (e/start %))]
                        :ends-during   [#(> from (e/end %)) #(> to (e/end %))]
                        :active-during [#(>= from (e/end %)) #(> to (e/start %))])
        slc           (into []
                            (comp
                             (drop-while drop?)
                             (take-while take?))
                            loop)]
    slc))



(defn offset [cycl amount]
  (->
   (map #(update % :start (partial + amount)) cycl)
   (with-meta (meta cycl))))


(defn lcp [cycls]
  (reduce lcm (map period cycls)))


(comment
  ;; 12
  (lcp [[(e/->event :a 0 1 0 2)] [(e/->event :b 0 1 2 3) (e/->event :c 1 1 1 3)] [(e/->event :d 2 1 3 4)]]))


(defn normalize-periods
  [cycls]
  (let [p (lcp cycls)]
    (map #(loop-cycl (/ p (period %)) %) cycls)))



(comment
  (normalize-periods [[(e/->event :a 0 1 0 2)] [(->event :b 0 1 0 3) (->event :c 1/2 1 1 3)] [(->event :d 0 1 3 4)]]))


(defn interleave-cycles
  [cycls]
  (->> (normalize-periods cycls)
       (apply concat)
       sort-cycl))


(comment
  (interleave-cycles [[(->event :a 0 1 2)] [(->event :b 0 1 3) (->event :c 1 1 3)] [(->event :d 2 1 4)]]))


(defn normalize
  "Takes a potentially nested collection of events of differing periods
  and creates a flat list of events of a single period."
  [cycl]
  (->
   (if-not (seq cycl)
     []
     (->> cycl
          sort-cycl
          (map vector)
          interleave-cycles))
   (with-meta (meta cycl))))


(comment
  (normalize [{:start 1/4 :length 1/4 :period 1/4} {:start 1/2 :length 1/4 :period 1/4}])
  (normalize [{:start 1/4 :length 1/4 :period 1}]))


(defn start [cycl]
  (-> cycl sort-cycl first e/start))


(defn end [cycl]
  (apply max (map e/end cycl)))


(defn length [cycl]
  (- (end cycl) (start cycl)))


(defn shape
  [cycl]
  ((juxt start length period) cycl))


(comment
  (shape [{:start 1/2 :length 1/2 :period 2} {:start 1/4 :length 1 :period 3}]))


(defn zero [cycl]
  (let [cycl (sort-cycl cycl)]
    (offset cycl (- (start cycl)))))


(comment
  (normalize
   [(->event :a 0 1 1) [(->event :b 1/2 1 2) (->event :c 3/2 1 2)] [(->event :d 3/4 1 1)]]))



(defn scale
  "Slows a cycle by a factor of x (or speeds it up if (< x 1)."
  [x cycl]
  (let [xer #(* x %)]
    (map (fn [evt]
           (-> evt
               (update :start xer)
               (update :length xer)
               (update :period xer)))
         cycl)))


(defn translate
  ([cycl x]
   (translate cycl 0 x x))
  ([cycl beg len p]
   (let [md (meta cycl)
         cycl       (normalize cycl)
         orig-start (start cycl)
         orig-len   (length cycl)
         factor     (/ len orig-len)]
     (->
      (for [evt cycl]
        (-> evt
            (update :start #(+ beg (* (- % orig-start) factor)))
            (update :length #(* % factor))
            (assoc :period p)))
      (with-meta md)))))


(comment
  (let [a #(rand-int 10)]
    [(realize a nil) (realize a nil)]))
