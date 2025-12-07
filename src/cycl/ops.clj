(ns cycl.ops
  "(👁️)"
  (:require
   [cycl.pattern :as p :refer [spin ->pat]]
   [cycl.util :as u :refer [smart-splat collate]]
   [cycl.merge :as m]
   [cycl.music :as mu]
   [cycl.event :as e]))


;; ops

(defn fit [& pat]
  (p/->Fit (map ->pat pat)))

(defn cyc [& pat]
  (p/->Cyc (map ->pat pat)))

(defn x [n & pat]
  (p/->OpMerge p/->Times (->pat n) (->pat pat)))

(defn el [x & pat]
  (p/->OpMerge p/->Elongate (->pat x) (->pat pat)))

(defn spl [& pat]
  (p/->Splice (map ->pat pat)))

(defn rep [n & pat]
  (p/->OpMerge p/->Repeat (->pat n) (->pat pat)))

(defn degrade [p & pat]
  (p/->OpMerge p/->Degrade (->pat p) (->pat pat)))

(defn may [p & pat]
  (p/->OpMerge p/->Maybe (->pat p) (->pat pat)))

(defn pick [& pats]
  (p/->Pick (map ->pat pats)))

(defn fast [x & pat]
  (p/->Speed x (->pat pat)))

(defn slow [x & pat]
  (p/->Speed (/ x) (->pat pat)))

(defn euc
  "Euclidian rhythm of `k` active of `n` switches, optionally rotated by `r`."
  [[k n & [r]] & pat]
  (p/->Euclid k n r (->pat pat)))

(defn stack
  [& pats]
  (p/->Stack (->pat pats)))


;; Controls

;; Control xfns


(defn rest? [v]
  (or (nil? v) (#{:- "~"} v)))


(defn parse-note
  [n]
  (cond
    (rest? n)    nil
    (keyword? n) (mu/note n)
    (string? n)  (mu/note n)
    (number? n)  (float n)
    :else        n))


(defn parse-sound
  [s]
  (cond
    (rest? s)    nil
    (keyword? s) (name s)
    :else        s))



(defn ->ctrl
  [sym xfn pat]
  (p/->Control sym xfn (->pat pat)))
                                

(defn n [& pat]
  (->ctrl :n float pat))


(defn s [& pat]
  (->ctrl :s parse-sound pat))



(defn f
  "Timed fns"
  [& pat]
  ;; TODO
  (->ctrl :fn (fn [v] (fn [merge-v ctx] #(v))) pat))


(defn s
  "Samples and synths"
  [& pat]
  (->ctrl :s p/parse-sound pat))


(defn mnt
  "Midi notes"
  [& pat]
  (->ctrl :note p/parse-note pat))


(defn nt
  "Notes"
  [& pat]
  (->ctrl :note #(- (p/parse-note %) 60) pat))


(comment
  (-> [(euc [3 5] :d) (pick :a :b) [:b (may 1/2 :c)]] (view :init))
  (-> [(euc [3 5] :d) (pick :a :b) [:b (may 1/2 :c)]] evts)
  (-> (n (euc [3 5] :d) (pick :a :d) [:b (may 1/2 :c)]) (view :n))

  (-> (n [(e/->event [:a :b] 0 1 1)]) evts)
  )


(defn pan
  "Left 0.0, Right 1.0"
  [& pat]
  (->ctrl :pan #(-> % (min 1) (max 0) float) pat))


(defn decay
  [& pat]
  (->ctrl :decay float pat))


(defn voice
  [& pat]
  (->ctrl :voice float pat))


(defn octave
  [& pat]
  (->ctrl :octave int pat))


(defn accelerate
  [& pat]
  (->ctrl :accelerate float pat))


(defn speed
  "Left 0.0, Right 1.0"
  [& pat]
  (->ctrl :speed float pat))


(defn vowel
  ":a :e :i :o :u"
  [& pat]
  (->ctrl :vowel name pat))


(defn room
  "Reverb room size"
  [& pat]
  (->ctrl :room float pat))


(defn size
  "Reverb size"
  [& pat]
  (->ctrl :size float  pat))


(defn dry
  "Reverb dry"
  [& pat]
  (->ctrl :dry float pat))


(defn legato
  "Play note for `n` segments, then cut."
  [& pat]
  (->ctrl :legato float pat))


;; Start here

(defn f| [f & pats]
  (p/->EventMerge (m/merge-events-split f) pats))


(comment

  (-> (f| m/left-merge (s :a) (s :b)) spin)
  (-> (f| m/left-merge [:a] [:b]) spin)
  (-> (f| m/left-merge :a :b) spin)
  (-> (f| m/left-merge (s :a) (n :b)) spin)
  (-> (f| m/left-merge [:a] [:b]) spin)

  (-> (f| (m/fn-merge vector) (s :a) (n :b)) spin)
  (-> (f| (m/fn-merge vector) (s :a) (s :b)) spin)
  (-> (f| (m/fn-merge #(and %1 %2)) [1 2 nil] [:a :b :c]) spin)

  (-> (f| (m/fn-merge m/stack-merge) [1] [2]) spin)

  (-> (f| m/or-merge [1 2 nil] [:a :b :c]) spin)
  (-> (f| m/or-merge (fit 1 2 nil) [:a :b :c]) spin)

  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) spin)
  (-> (f| m/apply-merge [60 61 62] [#(* 2 %)]) spin)
  (-> (f| m/apply-merge [60 61 62] [inc inc #(* 2 %)]) spin)
  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) spin)



  (-> (f| (m/apply|fn-merge vector) [1 2 3] [inc #(* 2 %) 4]) spin)
  (-> (f| m/apply|left-merge [:a :b] [name :c]) spin)
  (-> (f| m/apply|stack-merge [:a :b] [name :c] [:d :e]) spin)

  (-> (f| m/apply|stack-merge (s 1) (s :b)) spin)
  (-> (f| m/apply|stack-merge (n 1) (s :b)) spin)


  (-> (f| (m/apply|maths|or|stack-merge +) [6 6 6] [2 inc :d]) spin)
  (-> (f| (m/apply|maths|or|stack-merge -) [nil 0 nil] [2 inc :d]) spin)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc :d]) spin)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc name]) spin)


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

