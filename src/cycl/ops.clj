(ns cycl.ops
  "(👁️)"
  (:require
   [cycl.pattern :as p :refer [spin ->pat]]
   [cycl.merge :as m]
   [cycl.music :as mu]
   [cycl.event :as e]
   [cycl.cycl :as c]
   [cycl.val :as v]))


(defn spin*
  [pat]
  (-> pat spin (c/realize-cycl {})))

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
  (p/->Stack (map ->pat pats)))

(defn chop [n & pat]
  (p/->OpMerge p/->Chop (->pat n) (->pat pat)))

(defn striate [n & pat]
  (p/->OpMerge p/->Striate (->pat n) (->pat pat)))

(defn striate-by
  "Striate with adjustable slice length. `n` slices, each `len` of sample."
  [[n len] & pat]
  (p/->StriatBy n len (->pat pat)))

(defn rev [& pat]
  (p/->Reverse (->pat pat)))


(comment
  (spin
   (rev 1 2 3 4))

  (-> (jux rev (n 1 2 3 4)) spin*))


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
  [kw xfn pat]
  (p/->Control kw xfn (->pat pat)))
                                

(defn n [& pat]
  (->ctrl :n float pat))


(defn s [& pat]
  (->ctrl :s parse-sound pat))


(defn param [kw & pat]
  (->ctrl kw identity pat))


(defn f
  "Timed fns"
  [& pat]
  ;; TODO
  (->ctrl :fn (fn [v] (fn [merge-v ctx] #(v))) pat))


(defn s
  "Samples and synths"
  [& pat]
  (->ctrl :s parse-sound pat))


(defn mnt
  "Midi notes"
  [& pat]
  (->ctrl :note parse-note pat))


(defn nt
  "Notes"
  [& pat]
  (->ctrl :note #(- (parse-note %) 60) pat))


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


(comment
  ;; vfns
  ;; nondeterminism (nd)
  (-> (fit #(rand)) spin*)

  ;; application (appl)
  (-> (f| m/apply-merge (fit 2) (fit inc #(* 2 %) (constantly :a))) spin*)
  ;; NOTE: Supplied nil of no apply value available

  ;; merge fn (mfn)
  (-> (f| (m/fn-merge +) (fit 1 2 3) (fit 4 5) (fit 6)) spin*)

  ;; appl + nd
  (-> (f| m/apply-merge (fit #(rand-int 10)) (fit inc #(* 2 %))) spin*) ;; TODO: better if it is the same rand result?

  ;; mfn + appl
  (-> (f| (m/apply|fn-merge +) (fit 2 3 4) (fit inc 5 #(* 2 %))) spin*)
  ;; 
  ;; mfn + nd
  (-> (f| (m/fn-merge +) (fit #(rand-int 10)) (fit 3)) spin*)

  ;; mfn + appl + nd
  (-> (f| (m/apply|fn-merge +) (fit #(rand-int 10) #(rand-int 100)) (fit inc 50)) spin*)
  (-> (f| (m/apply|fn-merge +) (fit #(rand-int 10)) (fit #(* 2 %)) (fit -10)) spin*)

  ;; context fn (cfn)
  (let [tscale (fn [_ ctx] (-> ctx :event :start (* 2)))]
    (-> (fit tscale tscale tscale) spin*))

  (let [debug (fn [_ ctx] ctx)]
    (-> (fit debug debug) spin*))

  ;; cfn + appl
  ;; cfn + appl + nd
  ;; cfn + appl + nd + mfn
  ;; time fn (tfn)
  ;; multiple applications?

  )

(defn f| [f & pats]
  (p/->EventMergeSplit f (map ->pat pats)))



(comment

  (spin* (f| +))
  (->pat nil)

  (-> (f| m/left-merge (s :a) (s :b)) spin)
  (-> (f| m/left-merge [:a] [:b]) spin)
  (-> (f| m/left-merge :a :b) spin)
  (-> (f| m/left-merge (s :a) (n 1)) spin)
  (-> (f| m/left-merge [:a] [:b]) spin)

  (-> (f| (m/fn-merge vector) (s :a) (n 1)) spin)
  (-> (f| (m/fn-merge vector) (s :a) (s :b)) spin)
  (-> (f| (m/fn-merge vector) (s :a) (s :b)) spin*)
  (-> (f| (m/fn-merge #(and %1 %2)) [1 2 nil] [:a :b :c]) spin*)

  (-> (f| (m/fn-merge m/stack-merge) [1] [2]) spin)

  (-> (f| m/or-merge [1 2 nil] [:a :b :c]) spin*)
  (-> (f| m/or-merge (fit 1 2 nil) [:a :b :c]) spin*)

  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) spin*)
  (-> (f| m/apply-merge [60 61 62] [#(* 2 %)]) spin*)
  (-> (f| m/apply-merge [60 61 62] [inc inc #(* 2 %)]) spin*)
  (-> (f| m/apply-merge [60 61 62] [inc #(* 2 %)]) spin)


  :dbg
  (-> (f| m/apply-merge [0] [inc]) spin)

  (-> (f| m/apply|stack-merge [v/rand1]) spin*)
  (-> (f| m/apply|stack-merge [1 5 10] [v/randn]) spin*)
  (-> (f| m/apply|stack-merge [{:ampl 2}] [v/rand*]) spin*)
  (-> (f| m/apply|stack-merge [2] [v/rand*]) spin*)


  (-> (f| m/apply|stack-merge [v/sin1]) spin*)

  (-> (f| m/apply|stack-merge [1] [#(v/sin :a %)]) spin*)
  (-> (f| m/apply|stack-merge [5] [#(v/sin :a %)]) spin*)

  (-> (f| m/apply|stack-merge [{:ampl 1}] [(v/sin)]) spin*)
  (-> (f| m/apply|stack-merge [{:ampl 2}] [(v/sin)]) spin*)
  (-> (f| m/apply|stack-merge [{:ampl 2} {:ampl 3} {:ampl 4}] [v/rand*]) spin*)




  (-> (f| (m/apply|fn-merge vector) [1 2 3] [inc #(* 2 %) 4]) spin)
  (-> (f| m/apply|left-merge [:a :b] [name :c]) spin)
  (-> (f| m/apply|stack-merge [:a :b] [name :c] [:d :e]) spin)

  (-> (f| m/apply|stack-merge (s 1) (s :b)) spin)
  (-> (f| m/apply|stack-merge (n 1) (s :b)) spin)


  (-> (f| (m/apply|maths|or|stack-merge +) [6 6 6] [2 inc :d]) spin*)
  (-> (f| (m/apply|maths|or|stack-merge -) [nil 0 nil] [2 inc :d]) spin)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc :d]) spin)
  (-> (f| (m/apply|maths|or|stack-merge +) [nil 0 :c] [2 inc name]) spin)


  ,)

(comment
  (fit 0) [(+ 1) (+ 3) (+ 10)])

(defn f> [f & pats]
  (p/->EventMergeLeft f (map ->pat pats)))


(defn <f [f & pats]
  (p/->EventMergeLeft f (map ->pat (reverse pats))))


(defn <| [& pats]
  (apply f| m/left-merge pats))


(defn |> [& pats]
  (apply f| m/left-merge (reverse pats)))


(comment
  (spin (<| (s :a :b :c) (s :c :d :e)))
  (spin (|> (s :a :b :c) (s :c :d :e)))
  (spin (|> (s :a :b :c) (n 1 2 3))))


(defn a| [& pats]
  (apply f| m/apply-merge pats))



(defn s| [& pats]
  (apply f| m/apply|stack-merge pats))


(comment
  (-> (s| (n :a) (n #(rand-nth [:b :c :d]))) evts)
  (evts (s| [1 2 3] [:a :b :c] [4 5 6]))
  (-> (s| (n 1) (n inc) (n 2)) evts))


(defn m| [f & pats]
  (apply f| (m/apply|maths|or|stack-merge f) pats))


(comment
  (-> (m| + (n 1) (n 2)) spin*)
  (-> (m| + (n v/rand1) (n 2)) spin*)
  (-> (m| + (n (v/randn 10)) (n #(inc (/ % 2)))) spin)
  (-> (m| + (nt :a) (n 4)) spin*)
  (-> (m| + (s :a) (s :b)) spin*)
  (-> (m| + (nt :a) (s :b)) spin*)
  ,)


(defn m> [f & pats]
  (apply f> (m/apply|maths|or|stack-merge f) pats))


(defn <m [f & pats]
  (apply <f (m/apply|maths|or|stack-merge f) pats))


(defn +| [& pats]
  (apply m| + pats))


(comment
  (spin* (s| [2 4 6]
             [10 inc 8]
             [:a :b :c]))
  (spin* (s| (n 2 4 6)
             (n 10 9 8)
             (nt :a :b :c)))

  (spin*
   (+| [1 2 3] [4 5 6]))

  (spin* (s| (+| [1 2 3] [4 5 6])
             [:a :b :c]
             [:d :e :f]))


  (spin* (s| [:a :b :c]
             [:d :e :f]
             (+| [1 2 3] [4 5 6])))

  (spin* (f| m/apply|stack-merge
            [:a :a :a]
            (f| (fn [a b] (fn [_ ctx] (+ a b))) [1 3 5] [2 4 6])
            ))

  (spin*
   (+| (nt :c :d :e) (nt :c :d :e)))

  (spin* (s| (+| [1 2 3] [1 2 3])
             (m| - [10 10 10] (range 3))
             [:a :b :c])))


(defn +> [& pats]
  (apply m> + pats))


(defn <+ [& pats]
  (apply <m + pats))



(defn jux [tx pat]
  (stack (|> pat (pan 0))
         (|> (tx pat) (pan 1))))



(comment
  (evts
   (jux identity (n 1 2 3))))


;; fn-vals

(comment
  (spin
   (<| (fit :a) (fit :b :c))))
