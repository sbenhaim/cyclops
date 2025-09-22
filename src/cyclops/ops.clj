(ns cyclops.ops
  (:require
   [cyclops.pattern :as p :refer [->op]]
   [cyclops.util :as u :refer [smart-splat collate]]
   [cyclops.merge :as m]
   [cyclops.events :as e]))


(defn evts [rable]
  (e/realize rable nil))


(defn fit
  "Squeezes it's children into the confining space."
  [& children]
  (->op p/->FitOp children))


(comment
  (require '[dev-utils.portal :refer [tap-type>]])
  (require '[cyclops.viz :refer [vega-cycl]])

  (defn view
    ([c] (view c :auto))
    ([c param]
     (tap-type> :vega-lite (vega-cycl c param))))
  ,)



(cond-> (fit :a :b :c) )

(defn x [n-pat & children]
  (->op p/->TimesOp n-pat children))


(comment
  (->> [:a :b] (x 2) evts)
  (->> [:a (x 2 :b) :c] evts)
  (->> [:a :b] (x [2 2]) evts)
  (->> [:a :b] (x [2 2 1]) evts)

  (->> [:a :b] (x 2) view)
  (->> [:a (x 2 :b) :c] view)
  (->> [:a :b] (x [2 2]) view)
  (->> [:a :b] (x [2 2 1]) view)

  (e/realize (s :a :b) nil)


  )


(defn spl
  "Splices events into parent context adjusting segmentation."
  [& children]
  (->op p/->SpliceOp children))


(comment
  (-> [:a :b (spl :c :d)] view))


(defn rep [n-pat & children]
  (->op p/->RepOp n-pat children))


(comment
  (evts (rep 2 :sd))
  (evts (rep [2 2 1] :sd :bd))
  (evts [:a (rep [2 3] :b :c) :d])

  (view (rep 2 :sd))
  (view (rep [2 2] :sd :bd))
  (view [:a (rep [1 2] :b :c) :d])

  ,)


(defn slow [x-pat & children]
  (->op p/->SlowOp x-pat children))



(comment

  (evts (slow [2 2] :bd :sd))
  (evts (slow 2 :bd :sd))
  (evts (slow [2 1 2] :bd :sd :cr))

  (view (slow [2 2] :bd :sd))
  (view (slow 2 :bd :sd))
  (view (slow [2 1 2] :bd :sd :cr))
  )


(defn cyc
  "Stretches children across n cycles."
  [& children]
  (->op p/->CyclOp children))


(comment
  (->> (cyc :a [:b :c]) evts)
  (->> (cyc :a [:b :c]) view)
  (view [(cyc :a :c) :b])
  )


(defn may
  [x-pat & children]
  (p/->MaybeOp (u/ensure-vec x-pat) (u/smart-splat children) :per))


(defn may*
  [x-pat & children]
  (p/->MaybeOp (u/ensure-vec x-pat) (u/smart-splat children) :all))



(comment
  (evts (may [0.5] [[:a :b]]))
  (evts (may* [0.5] [[:a :b]]))
  (e/realize (may [1 0.5] :a :b) nil))


(defn euc
  "Euclidian rhythm of `k` active of `n` switches, optionally rotated by `r`."
  [[k n & [r]] & children]
  (p/->EuclidOp k n r (smart-splat children)))


(comment
  (evts (euc [3 4] :a))
  (view (euc [3 4] :a))
  (view (euc [5 8] [:a :b [:c :d]])))

(defn pick
  "Each loop, randomly chooses one of its children."
  [& children]
  (->op p/->PickOp children))


(comment
  (def ptop (atom nil))
  (tap> ptop)
  (reset! ptop (with-meta (vega-cycl (p/->cycle [:a :b :c]))
                 {:portal.viewer/default :portal.viewer/vega-lite}))
  (def cycl (atom (vega-cycl (p/->cycle [:a (pick :b :c)]) :init)))
  (tap> cycl)
  (reset! cycl (vega-cycl (p/->cycle [:a :b :c ])))
  (evts [:a (pick :b :c)]))


(defn el
  [n-pat & children]
  (->op p/->ElongateOp n-pat children))


(comment
 (view [:a (el 2 :b)]))


(defn stack
  "Plays contained patterns or events simultaneously. Can be used to play chords."
  [& children]
  (->op p/->StackOp children))


(comment
  (view (stack [:a :b] [nil :c :d])))


(defn rev
  "TODO: This"
  [& children]
  (spl (reverse (smart-splat children))))


(comment
  ;; API

  (-> [:a [:b :c]] fit rev) ;; Applied to pattern => Maybe reverse on children? Nonrecursive by default
  (-> [:a [:b :c]] rev fit) ;; Applied to collection => Just `reverse`
  (-> (+| (n :c) (mu/chord :cm7) (s :superpiano)) rev) ;; Applied to Cycle. Recursive
  (-> (+| (n :c) (rev (mu/chord :cm7)) (s :superpiano))) ;; Applied to collection


  (defprotocol Reversible
    (rev [this]))

  (extend-protocol Reversible
    clojure.lang.Sequential
    (rev [this] (reverse this))
    cyclops.pattern.Operatic
    (rev [this] (update this :children reverse)) ;; Assume `:children`. Override in particular Ops
    cyclops.events.Cyclic
    (rev [this] (reverse-cycl this)))

 )


(comment
  evts
  (p/operate
   (rev :a :b :c)
   p/base))

;; Controls

(defn s
  "Samples and synths"
  [& pat]
  (p/->ctrl :s p/parse-sound (smart-splat pat)))


(comment
  (s :c :a :f :e)
  (evts (s (rep 2 :sd :bd))))


(defn n
  "Numbers and notes."
  [& pat]
  (p/->ctrl :n p/parse-num (smart-splat pat)))


(comment
  (-> [(euc [3 5] :d) (pick :a :b) [:b (may 1/2 :c)]] view)
  (-> [(euc [3 5] :d) (pick :a :b) [:b (may 1/2 :c)]] evts)
  (-> (n (euc [3 5] :d) (pick :a :d) [:b (may 1/2 :c)]) (view :s)))


(defn pan
  "Left 0.0, Right 1.0"
  [& pat]
  (p/->ctrl :pan (collate float) (smart-splat pat)))


(defn vowel
  ":a :e :i :o :u"
  [& pat]
  (p/->ctrl :vowel (collate name) (smart-splat pat)))


(comment (view (vowel :a [:e :i :o] :u)))


(defn room
  "Reverb room size"
  [& pat]
  (p/->ctrl :room float (smart-splat pat)))


(defn size
  "Reverb size"
  [& pat]
  (p/->ctrl :size float (smart-splat pat)))


(defn dry
  "Reverb dry"
  [& pat]
  (p/->ctrl :dry float (smart-splat pat)))


(defn legato
  "Play note for `n` segments, then cut."
  [& pat]
  (p/->ctrl :legato float (smart-splat pat)))


#_(defn rev-cycl
  "Reverse cycle"
  [& cycl]
  (e/q-cycle-xf cycl (fn [evts]
                       (->> evts
                            (map (fn [e] (update e :start #(- (e/period cycl) %))))
                            (sort e/event-sort)))))



(comment
  (-> [:a [:b :c]] p/->cycle e/rev-cycl evts))


(defn f| [f & cycles]
  (m/merge-cycles* f (smart-splat cycles)))


(comment

  (-> (f| m/left-merge (s :a) (s :b)) evts)
  (-> (f| m/left-merge (s :a) (n :b)) evts)

  (-> (f| (m/fn-merge vector) (s :a) (n :b)) evts)
  (-> (f| (m/fn-merge vector) (s :a) (s :b)) evts)


  (-> (f| m/and-merge [1 2 nil] [:a :b :c]) evts)
  (-> (f| m/or-merge [1 2 nil] [:a :b :c]) evts)


  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) evts)
  (-> (f| m/apply-merge [60 61 62] [#(* 2 %)]) evts)
  (-> (f| m/apply-merge [60 61 62] [inc inc #(* 2 %)]) evts)
  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) evts)



  (-> (f| (m/apply|fn-merge vector) [1 2 3] [inc #(* 2 %) 4]) evts)
  (-> (f| m/apply|left-merge [:a :b] [name :c]) evts)
  (-> (f| m/apply|stack-merge [:a :b] [name :c]) evts)

  (-> (f| m/apply|stack-merge (n 1) (s :b)) evts)


  (-> (f| (m/apply|maths|or|stack-merge +) [6 6 6] [2 inc :d]) evts)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 nil] [2 inc :d]) evts)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc :d]) evts)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc name]) evts)


  ,)


(defn f> [f & cycles]
  (m/merge-cycles* f (smart-splat cycles) :left-merge))


(defn <f [f & cycles]
  (m/merge-cycles* f (reverse (smart-splat cycles)) :left-merge))


(defn s| [& cycles]
  (apply f| m/apply|stack-merge cycles))


(comment
  (-> (s| (n :a) (n #(rand-nth [:b :c :d]))) evts)
  (-> (s| (n 1) (n inc) (n 2)) evts))


(defn m| [f & cycles]
  (apply f| (m/apply|maths|or|stack-merge f) cycles))


(comment
  (-> (m| + (n 1) (n 2)) evts)
  (-> (m| + (n #(rand-int 10)) (n 2)) evts)
  (-> (m| + (n #(rand-int 10)) (n #(inc (/ % 2)))) evts)
  (-> (m| + (n :a) (n 4)) evts)
  (-> (m| + (s :a) (s :b)) evts)
  (-> (m| + (n :a) (s :b)) evts)
  ,)


(defn m> [f & cycles]
  (apply f> (m/apply|maths|or|stack-merge f) cycles))


(defn <m [f & cycles]
  (apply <f (m/apply|maths|or|stack-merge f) cycles))


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
