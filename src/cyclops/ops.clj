(ns cyclops.ops
  (:require
   [cyclops.pattern :as p :refer [->op]]
   [cyclops.util :as u :refer [smart-splat collate cycle-n]]
   [cyclops.merge :as m]
   [cyclops.events :as e]))


(defn evts [rable]
  (e/realize rable nil))


(defn fit
  "Squeezes it's children into the confining space."
  [& children]
  (->op p/->FitOp children))


(defn x [n-pat & children]
  (->op p/->TimesOp n-pat children))


(defn spl
  "Splices events into parent context adjusting segmentation."
  [& children]
  (->op p/->SpliceOp children))


(defn rep [n-pat & children]
  (->op p/->RepOp n-pat children))



(defn slow [x-pat & children]
  (->op p/->SlowOp x-pat children))


;; TODO: This doesn't look right
(comment
 (evts
  (slow [1 2] [:a :b])))


(defn cyc
  "Stretches children across n cycles."
  [& children]
  (slow (p/sum-weights children) children))


(comment
  (->
   (cyc :a :b :c)
   evts))


#_(defn slow-ctrl
    "Numbers and notes."
    [& pat]
    (p/->control :slow (collate identity) (smart-splat pat)))


(defn slow+
  "Stretches children across n cycles."
  [arg-pat & children]
  (apply f| (m/apply|maths|stack-merge *) (slow-ctrl arg-pat) children))



#_(comment
  (slow-ctrl 1 2 [3 4])
  (slow+ [1 2] [:a :b]) ;; ???
  (evts
   (slow+ [1 2 [3 4]] (n :a :b :c))))


(do
  (defn may
    [x & children]
    (p/->Op :maybe {:x x} (smart-splat children)))
  (defmethod p/operate :maybe
    [op ctx]
    (let [x (-> op p/args :x)]
      (p/splice (map (fn [e] #(when (< (rand) x) e)) (p/children op)) ctx)))
  (defmethod p/weigh :maybe
    [op]
    (p/sum-weights (p/children op))))


(do
  (defn euc
    "Euclidian rhythm of `k` active of `n` switches, optionally rotated by `r`."
    [[k n & [r]] & val]
    (p/->Op :euclid {:k k :n n :r r} (smart-splat val)))
  (defmethod p/operate :euclid
    [op ctx]
    (let [{:keys [k n r]} (p/args op)
          mask     (p/bjork (repeat k [true]) (repeat (- n k) [nil]))
          mask     (u/rot mask (or r 0))
          children (map #(and % val) mask)]
      (p/splice children ctx)))
  (defmethod p/weigh :euclid
    [op] (* (-> op p/args :n)
            (p/weigh (p/children op)))))


(do
  (defn pick
    "Each loop, randomly chooses one of its children."
    [& children]
    (p/->Op :pick nil (smart-splat children)))
  (defmethod p/operate :pick
    [op ctx]
    (p/splice [#(rand-nth (p/children op))] ctx))
  (defmethod p/weigh :pick
    [op]
    (p/weigh (first (p/children op)))))


(do
  (defn el
    "Stretches note across `n` segments."
    [n & children]
    (p/->Op :elongate {:n n} (smart-splat children)))
  (defmethod p/operate :elongate
    [op {:keys [segment-length] :as context}]
    (let [children       (p/children op)
          n              (p/sum-weights children)
          segment-length (/ segment-length n)
          spacing        segment-length]
      (p/apply-timing children (assoc context
                                      :segment-length segment-length
                                      :spacing spacing)))))


(do
  (defn stack
    "Plays contained patterns or events simultaneously. Can be used to play chords."
    [& children]
    (p/->Op :stack nil (smart-splat children)))
  (defmethod p/operate :stack
    [op ctx]
    (mapcat
     #(p/operate % ctx)
     (map vector (p/children op))))
  (defmethod p/weigh :stack
    [op]
    (apply max (map p/weigh (p/children op)))))


;; Controls

(defn s
  "Samples and synths"
  [& pat]
  (p/->control :s p/parse-sound (smart-splat pat)))


(defn n
  "Numbers and notes."
  [& pat]
  (p/->control :n p/parse-num (smart-splat pat)))


(comment (-> (n :a) evts))


(defn pan
  "Left 0.0, Right 1.0"
  [& pat]
  (p/->control :pan (collate float) (smart-splat pat)))


(defn vowel
  ":a :e :i :o :u"
  [& pat]
  (p/->control :vowel (collate name) (smart-splat pat)))


(defn room
  "Reverb room size"
  [& pat]
  (p/->control :room float (smart-splat pat)))


(defn size
  "Reverb size"
  [& pat]
  (p/->control :size float (smart-splat pat)))


(defn dry
  "Reverb dry"
  [& pat]
  (p/->control :dry float (smart-splat pat)))


(defn legato
  "Play note for `n` segments, then cut."
  [& pat]
  (p/->control :legato float (smart-splat pat)))


(defn rev-cycl ;; TODO: How to unify? Also, this doesn't work.
  "Reverse cycle"
  [& cycl]
  (e/q-cycle-xf cycl (fn [evts]
                       (->> evts
                            (map (fn [e] (update e :start #(- (e/period cycl) %))))
                            (sort e/event-sort)))))


(defn rev-pat
  "Reverse pat"
  [& pat]
  (reverse (smart-splat pat)))



(defn f| [f & cycles]
  (m/merge-cycles f (smart-splat cycles)))


(comment
  (->
   (f| (m/apply|fn-merge #(u/vector* (:prev %) (:cur %)))
       (n :a)
       (n :b))
   evts)

  (->
   (f| m/apply|left-merge
       (n :a)
       (n :b))
   evts)

  (->
   (f| m/stack-merge
       (n 1)
       (n inc))
   evts)

  (->
   (f| m/apply|stack-merge
       (n :a)
       (n #(inc (:prev %)))
       (n :b))
   evts)

  (->
   (f| m/apply|stack-merge
       (n 1)
       (n #(inc 2)))
   evts)


  (->
   (f| m/apply|stack-merge
       (n 1)
       (s :b))
   evts)


  (->
   (f| (m/apply|maths|stack-merge +)
       (s 1)
       (s #(inc (:prev %)))
       (s #(rand-int 10)))
   evts)


  ,)


(defn f> [f & cycles]
  (m/merge-cycles f (smart-splat cycles) :left))


(defn <f [f & cycles]
  (m/merge-cycles f (reverse (smart-splat cycles)) :left))


(defn s| [& cycles]
  (apply f| m/apply|stack-merge cycles))


(comment
  (->
   (s| (n :a)
       (n #(rand-nth [:b :c :d])))
   evts)

  (->
   (s| (n 1)
       (n #(inc (:prev %)))
       (n 2))
   evts))


(defn m| [f & cycles]
  (apply f| (m/apply|maths|stack-merge f) cycles))


(comment
  (-> (m| + (n 1) (n 2)) evts)
  (-> (m| + (n #(rand-int 10)) (n 2)) evts)
  (-> (m| + (n #(rand-int 10)) (n #(inc (/ (:prev %) 2)))) evts)
  (-> (m| + (n :a) (n 4)) evts)
  (-> (m| + (s :a) (s :b)) evts)
  (-> (m| + (n :a) (s :b)) evts)
  ,)


(defn m> [f & cycles]
  (apply f> (m/apply|maths|stack-merge f) cycles))


(defn <m [f & cycles]
  (apply <f (m/apply|maths|stack-merge f) cycles))


(defn +| [& cycles]
  (apply m| + cycles))


(defn +> [& cycles]
  (apply m> + cycles))


(defn <+ [& cycles]
  (apply <m + cycles))


(defn jux [tx cyc]
  (s|
   (+| cyc (pan 0))
   (+| (tx cyc) (pan 1))))


;; TODO: Pattern accepting time controls?
;; note("c2, eb3 g3 [bb3 c4]").sound("piano").slow("0.5,1,1.5")
;; Implementation could just be stack merge of three different slow pats
;; Suggests mix and match of cycle merges (stack) and patterns (slow)
;; Way to make pats and cycles more uniform, so things like `rev` and `slow`
;; Can act on both?

(defn x+
  "Stretches children across n cycles."
  [arg-pat & children]
  (apply f| (m/apply|maths|stack-merge *) arg-pat (x 1 children)))

(evts
 (f| (m/fn-merge +)
     [#(rand-int 10) 1]
     [#(rand-int 10) 2 3]))



(defn sin
  ([] (sin 0 1 1))
  ([max] (sin 0 max 1))
  ([min max] (sin min max 1))
  ([min max period]
   (fn [{:keys [event]}]
     (let [TAU  (* 2 Math/PI)
           mult (-> max (- min) (/ 2))
           base (-> (:start event) (* TAU) (/ period) Math/sin (+ 1) (* mult))
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
