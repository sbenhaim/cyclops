(ns cycl.ops
  "(👁️)"
  (:require
   [cycl.pattern :as p]
   [cycl.util :as u :refer [smart-splat collate]]
   [cycl.merge :as m]
   [cycl.music :as mu]
   [cycl.events :as e]))


(defmulti evts type)


(defmethod evts :default [rable]
  (e/realize rable nil))


(comment (evts [:a :b :c]))
(comment (evts (n :a :b :c)))


(defmethod evts cycl.pattern.Operatic [op]
  (-> op p/->cycl (e/realize nil)))


(defn ->op
  ([op children]
   (op (u/smart-splat children)))
  ([op argpat children]
   (let [kids (u/smart-splat children)
         args (u/gimme-vec argpat)]
     (op args kids))))


(defn fit
  "Squeezes it's children into the confining space."
  [& children]
  (->op p/->FitOp children))


(comment
  (require '[dev-utils.portal :refer [tap-type>]])
  (require '[cyclops.viz :refer [vega-cycl]])

  (defn view
    ([c] (view c nil))
    ([c param]
     (tap-type> :vega-lite (vega-cycl (evts c) param))))

  (evts (fit :a :b))

  (p/->cycl
   (fit :a (cyc :b (fit 2 :c))))

  (evts (fit :a (cyc :b (fit 2 :c))))

  (evts (p/basic-ctrl :s (fit :a (cyc :b (fit 2 :c)))))

  ,)


(defn x [n-pat & children]
  (->op p/->TimesOp* n-pat children))


(comment
  (evts (x 2 :a))
  (evts (cyc (x 2 :a))))

(defn -x [n & children]
  (->op p/->TimesOp n children))



(comment
  (evts (-x 2 (cyc :sd :bd))))


(defn chop
  [& numpat]
  (let [evts      (-> numpat u/smart-splat p/->cycl)
        timeses   (map #(x (e/get-init %) %) evts)
        pre-chops (p/->cycl timeses)]
    pre-chops
    #_(for [chp pre-chops]
        (-> chp
            (e/assoc-param :begin (:start chp))
            (e/assoc-param :end (e/end chp))
            (e/dissoc-param :init)))))


(chop 2)

(comment
  (evts (<| (chop 2) (s :bd))))


(comment

  (evts (x 2 :a))
  (evts (s (x 2 :a)))
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
  (->op p/->RepOp* n-pat children))


(comment
  (evts (rep 2 :sd))
  (evts (rep [2 2 1] :sd :bd))
  (evts [:a (rep [2 3] :b :c) :d])

  (view (rep 2 :sd))
  (view (rep [2 2] :sd :bd))
  (view [:a (rep [1 2] :b :c) :d])

  ,)


(defn slow [x-pat & children]
  (->op p/->SlowOp* x-pat children))



(comment

  (evts (slow 3/2 :bd))
  (evts (slow [2 2] :bd :sd))
  (evts (slow 2 :bd :sd))
  (evts (slow [2 1 2] :bd :sd :cr))

  (view (slow [2 2] :bd :sd))
  (view (slow 2 :bd :sd))
  (evts (slow [2 1 2] :bd :sd :cr))

  (evts (cyc (n :a :b :c)))
  (evts (fit (n :a :b :c)))
  (evts (euc [3 5] (n :a :b)))

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
  (p/->MaybeOp* (u/gimme-vec x-pat) (u/smart-splat children) :all))


(comment
  (->> (may [1 1/2] :a :b :c :d) evts (tap-type> :table)))


(defn degrade
  [x-pat & children]
  (p/->MaybeOp* (u/gimme-vec x-pat) (u/smart-splat children) :per))

(comment
  (->> (degrade [1/2] [:a :b :c :d]) evts (tap-type> :table))
  (->> (degrade [1 1/2] [:a :b :c :d]) evts (tap-type> :table)))

(comment
  (evts (may 0.5 :a :b)) ; per
  (evts (may 0.5 [[:a :b]])) ; all
  (evts (degrade 0.5 :a :b))
  (evts (may [1 0.5] :a :b))
  ;; same as
  (evts (may [1 0.5] :a :b)))


(defn euc
  "Euclidian rhythm of `k` active of `n` switches, optionally rotated by `r`."
  [[k n & [r]] & children]
  (p/->EuclidOp k n r (smart-splat children)))


(comment
  (-> (euc [5 8] :a) view)
  (let [p (atom false)]
    (-> (euc [5 8] (fn [] (swap! p not) (if @p :a :b)))
        n evts view))
  (-> (euc [5 8] :a :b) view)
  (-> (euc [5 8] [:a :b [:c :d]]) view))

(defn pick
  "Each loop, randomly chooses one of its children."
  [& children]
  (->op p/->PickOp children))


(comment
  (def ptop (atom nil))
  (tap> ptop)
  (reset! ptop (with-meta (vega-cycl (evts [:a :b :c]))
                 {:portal.viewer/default :portal.viewer/vega-lite}))
  (def cycl (atom (vega-cycl (p/->cycle [:a (pick :b :c)]) :init)))
  (tap> cycl)
  (reset! cycl (vega-cycl (p/->cycle [:a :b :c ])))
  (evts [:a (pick :b :c)]))


(defn el
  [n-pat & children]
  (->op p/->ElongateOp* n-pat children))


(comment
 (-> (fit :a (el 2 :b)) evts)
 (-> [:a (el 2 :b)] fit view)
 )


(defn stack
  "Plays contained patterns or events simultaneously. Can be used to play chords."
  [& children]
  (->op p/->StackOp children))


(def stk stack)


(comment
  (view (stack [:a :b] [nil :c :d]))
  (view (stk [:a :b] [nil :c :d])))


(defn rev
  [& children]
  (let [cycl (p/->cycl children)
        p    (e/period cycl)]
    (->> cycl
         (map (fn [e] (assoc e :start (- p (:start e) (:length e)))))
         sort)))


(comment

  (-> (rev :a :b :c) evts)
  (-> (rev [:a :b] :c) evts)
  (-> (rev (cyc [:a :b] :c)) evts)
  (-> [:a [:b :c]] fit rev evts)
  (-> [:a [:b :c]] rev fit evts) ;; Applied to collection => Just `reverse`. Nah, because [:a :b :c] should usually == (fit :a :b :c)

  ;; TODO
  (-> (+| (n (mu/chord :cm7)) (s :superpiano)) rev evts) ;; Applied to Cycle. Recursive
  (-> (+| (rev (mu/chord :cm7)) (s :superpiano)) evts) ;; Applied to collection


  )


;; Controls


;; Controls
;; TODO: Just move to op?


(defn ->cycl?
  "If you have good reason to believe somethign *should* be a `cycl`, use this
  function to make sure it is one."
  [thing]
  (cond
    (e/cycl? thing) thing
    (p/op? thing)   (p/->cycl thing)
    :else           (p/->cycl [thing])))


(defn ->param
  [param value-tx pat]
  (->> pat
       smart-splat
       ->cycl?
       (map (fn [evt] (e/reassoc-param
                       evt
                       :init
                       param
                       #(u/defer (u/collate value-tx) %))))))

(defn ->const
  [param val pat]
  (->> pat
       smart-splat
       ->cycl?
       (map (fn [evt] (assoc-in
                       evt
                       [:params param]
                       val)))))


(defn f
  "Timed fns"
  [& pat]
  (->> pat
       (->param :fn p/parse-sound)
       (->const :target :fn)))


(defn s
  "Samples and synths"
  [& pat]
  (->param :s p/parse-sound pat))


(comment
  (-> (s :c :a :f :e) rev evts)
  (-> [(stack :c :d) :a :f :e] s (view :s))
  (view (s (rep 2 :sd :bd))))


(defn n
  "Numbers"
  [& pat]
  (->param :n float pat))


(defn mnt
  "Midi notes"
  [& pat]
  (->param :note p/parse-note pat))


(defn nt
  "Notes"
  [& pat]
  (->param :note #(- (p/parse-note %) 60) pat))


(comment
  (-> [(euc [3 5] :d) (pick :a :b) [:b (may 1/2 :c)]] (view :init))
  (-> [(euc [3 5] :d) (pick :a :b) [:b (may 1/2 :c)]] evts)
  (-> (n (euc [3 5] :d) (pick :a :d) [:b (may 1/2 :c)]) (view :n))

  (-> (n [(e/->event [:a :b] 0 1 1)]) evts)
  )


(defn pan
  "Left 0.0, Right 1.0"
  [& pat]
  (->param :pan #(-> % (min 1) (max 0) float) pat))


(defn decay
  [& pat]
  (->param :pan float pat))


(defn voice
  [& pat]
  (->param :voice float pat))


(defn octave
  [& pat]
  (->param :octave int pat))


(defn accelerate
  [& pat]
  (->param :accelerate float pat))


(defn speed
  "Left 0.0, Right 1.0"
  [& pat]
  (->param :speed float pat))


(defn vowel
  ":a :e :i :o :u"
  [& pat]
  (->param :vowel name (smart-splat pat)))


(comment (view (vowel :a [:e :i :o] :u))
         (evts (vowel {:init [:a :b]} [:e :i :o] :u)))


(defn room
  "Reverb room size"
  [& pat]
  (->param :room float (smart-splat pat)))


(defn size
  "Reverb size"
  [& pat]
  (->param :size float (smart-splat pat)))


(defn dry
  "Reverb dry"
  [& pat]
  (->param :dry float (smart-splat pat)))


(defn legato
  "Play note for `n` segments, then cut."
  [& pat]
  (->param :legato float (smart-splat pat)))


(defn ->cycl?* [cycls]
  (map ->cycl? cycls))


(comment
  (->cycl?* [[:a :b :c] (cyc 1 2 3) [(e/->event :a 0 1 1)] :a]))


(defn f| [f & cycls]
  (m/merge-cycles* f (-> cycls ->cycl?*)))


(comment

  (-> (f| m/left-merge (s :a) (s :b)) evts)
  (-> (f| m/left-merge [:a] [:b]) evts)
  (-> (f| m/left-merge :a :b) evts)
  (-> (f| m/left-merge (s :a) (n :b)) evts)
  (-> (f| m/left-merge [:a] [:b]) evts)

  (-> (f| (m/fn-merge vector) (s :a) (n :b)) evts)
  (-> (f| (m/fn-merge vector) (s :a) (s :b)) evts)
  (-> (f| (m/fn-merge #(and %1 %2)) [1 2 nil] [:a :b :c]) evts)

  (-> (f| (m/fn-merge m/stack-merge) [1] [2]) evts)

  (-> (f| m/or-merge [1 2 nil] [:a :b :c]) evts)
  (-> (f| m/or-merge (fit 1 2 nil) [:a :b :c]) evts)

  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) evts)
  (-> (f| m/apply-merge [60 61 62] [#(* 2 %)]) evts)
  (-> (f| m/apply-merge [60 61 62] [inc inc #(* 2 %)]) evts)
  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) evts)



  (-> (f| (m/apply|fn-merge vector) [1 2 3] [inc #(* 2 %) 4]) evts)
  (-> (f| m/apply|left-merge [:a :b] [name :c]) evts)
  (-> (f| m/apply|stack-merge [:a :b] [name :c] [:d :e]) evts)

  (-> (f| m/apply|stack-merge (s 1) (s :b)) evts)
  (-> (f| m/apply|stack-merge (n 1) (s :b)) evts)


  (-> (f| (m/apply|maths|or|stack-merge +) [6 6 6] [2 inc :d]) evts)
  (-> (f| (m/apply|maths|or|stack-merge -) [nil 0 nil] [2 inc :d]) evts)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc :d]) evts)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc name]) evts)


  ,)


(defn f> [f & cycles]
  (m/merge-cycles* f cycles :left-merge))


(defn <f [f & cycles]
  (m/merge-cycles* f (reverse cycles) :left-merge))


(defn <| [& cycles]
  (apply f| m/left-merge cycles))


(defn |> [& cycles]
  (apply f| m/left-merge (reverse cycles)))


(comment
  (evts (<| (s :a :b :c) (s :c :d :e)))
  (evts (|> (s :a :b :c) (s :c :d :e)))
  (evts (|> (s :a :b :c) (n 1 2 3))))


(defn a| [& cycles]
  (apply f| m/apply-merge cycles))



(defn s| [& cycles]
  (apply f| m/apply|stack-merge cycles))


(comment
  (-> (s| (n :a) (n #(rand-nth [:b :c :d]))) evts)
  (evts (s| [1 2 3] [:a :b :c] [4 5 6]))
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


(comment
  (evts (s| [2 4 6]
            [10 inc 8]
            [:a :b :c]))
  (evts (s| (n 2 4 6)
            (n 10 9 8)
            (n :a :b :c)))

  (+| [1 2 3] [4 5 6])

  (evts (s| (+| [1 2 3] [4 5 6])
            [:a :b :c]
            [:d :e :f]))


  (evts (s| [:a :b :c]
            [:d :e :f]
            (+| [1 2 3] [4 5 6])))

  (evts (f| m/apply|stack-merge
            [:a :a :a]
            (f| (fn [a b] (fn [_ ctx] (+ a b))) [1 3 5] [2 4 6])
            ))

  (evts
   (+| (nt :c :d :e) (nt :c :d :e)))

  (evts (s| (+| [1 2 3] [1 2 3])
            (m| - [10 10 10] (range 3))
            [:a :b :c])))


(defn +> [& cycles]
  (apply m> + cycles))


(defn <+ [& cycles]
  (apply <m + cycles))


(defn jux [tx cyc]
  (s| (|> cyc (pan 0))
      (|> (tx cyc) (pan 1))))


(comment
  (evts
   (jux identity (n 1 2 3))))


;; fn-vals


(defn trig-fn
  [trig-fn pos period ampl]
  (-> pos             ;; What part of the cycl are we on
      (* 2 Math/PI)   ;; Maths
      (/ period)      ;; How many cycles to stretch over
      trig-fn         ;; Trig
      (+ 1)           ;; -1 to 1 => 0 to 2
      (* ampl)        ;; Amplitude
      (/ 2)))         ;; 0 to 2 => 0 to 1



(defn amp
  ([max] (amp 0 max))
  ([min max]
   (fn [v]
     (let [mult (-> max (- min))]
       (+ min (* mult (or v 0)))))))


(defn sin [ampl {:keys [event]}]
  (trig-fn Math/sin (:start event) (:period event) (or ampl 1)))


(defn cos [ampl {:keys [event]}]
  (trig-fn Math/cos (:start event) (:period event) (or ampl 1)))


(defn square [ampl {:keys [event]}]
  (let [half (/ (:period event) 2)]
       (if (< (:start event) half)
         0
         (or ampl 1))))

(map #(mod % 1) (range 0 3 0.1))

(defn saw
  [ampl {:keys [event]}]
  (let [{:keys [start period]} event]
    (mod (/ start period) (or ampl 1))))

(defn isaw
  [ampl ctx]
  (let [ampl (or ampl 1)]
    (- ampl (saw ampl ctx))))

(comment
  (map (partial saw 1) (for [s (range 0 2 0.2)] {:event {:start s :period 1}})))


(defn itri
  [ampl {:keys [event]}]
  (let [{:keys [start period]} event
        v (abs (- 1 (mod (* (/ 2 period) start) 2)))]
    (* (or ampl 1) v)))

(comment
  (map (partial itri 1) (for [s (range 0 2 0.1)] {:event {:start s :period 1}})))

(defn tri
  [ampl ctx]
  (let [ampl (or ampl 1)]
    (- ampl (itri ampl ctx))))


(comment
  (map (partial tri 2) (for [s (range 0 2 0.1)] {:event {:start s :period 2}})))



(defn rand
  ([ampl] (clojure.core/rand (or ampl 1))))


(defn irand
  [ampl]
  #(rand-int ampl))

