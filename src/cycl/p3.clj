(ns cycl.p3
  (:require [cycl.event :as e]
            [cycl.util :as u]
            [cycl.cycl :as c]
            [cycl.merge :as m]))


(defprotocol Op
  (slice [this start length])
  (period [this])
  (weight [this]))


(defn weigh
  [pattern]
  (reduce + (map weight pattern)))


(defn iter-start
  [cycle-no period]
  (* period (quot cycle-no period)))


(defn spin
  [pat]
  (let [p (period pat)]
    (slice pat 0 p)))


(defn spin*
  [pat]
  (-> pat spin (c/realize-cycl {})))


(defn lcp
  [pats]
  (if (= 1 (count pats))
    (period (first pats))
    (apply u/lcm (map period pats))))


(defrecord Pure [value]
  Op
  (slice [_ start length]
    (let [start (long (Math/ceil start))]
      (-> (map #(e/->event value % 1) (iterate inc start))
          (c/slice-starts start length))))
  (period [_] 1)
  (weight [_] 1))


(defrecord LitOp [events p w]
  Op
  (slice [this start length]
    (let [i-start (iter-start start (period this))]
      (->
       (mapcat
        (fn [cycl iter]
          (for [evt cycl]
            (-> evt (update :start #(+ % iter)))))
        (repeat events)
        (iterate #(+ % p) i-start))
       (c/slice-starts start length))))
  (period [_] p)
  (weight [_] w))


(defn arrange
  [tx-evt pats]
  (let [weights  (map weight pats)
        n        (reduce + weights)
        weights* (cycle weights)
        offsets* (reductions + 0 weights*)]
    (-> (mapcat
         (fn [pat weight offset]
           (let [sub-iter-no (quot offset n)
                 sub-pat     (slice pat sub-iter-no 1)]
             (for [evt sub-pat]
               (tx-evt evt offset weight n))))
         (cycle pats)
         weights*
         offsets*))))


(defrecord FitOp [patterns]
  Op
  (slice [this start length]
    (let [i-start (iter-start (long start) (period this))]
      (->
       (arrange
        (fn [evt offset weight n]
          (let [[_ frac] (u/mixed (e/start evt))
                scale    (/ n)]
            (-> evt
                (assoc :start (+ i-start (* offset scale) (* frac scale weight)))
                (update :length #(* % scale weight)))))
        patterns)
       (c/slice-starts start length))))
  (period [_] (lcp patterns))
  (weight [_] 1))


(defn ->pat [x]
  (cond
    (satisfies? Op x) x
    (sequential? x)   (cond
                        (c/cycl? x) (->LitOp x 1 1)
                        :else       (->FitOp (map ->pat x)))
    :else             (->Pure x)))



(defrecord CyclOp [patterns]
  Op
  (slice [this start length]
    (let [i-start (iter-start (long start) (period this))]
      (->
       (arrange
        (fn [evt offset weight n]
          (let [[_ frac] (u/mixed (e/start evt))]
            (-> evt
                (assoc :start (+ i-start offset (* frac weight)))
                (update :length #(* % weight)))))
        patterns)
       (c/slice-starts start length))))
  (period [_] (let [ps    (map period patterns)
                    top-p (weigh patterns)]
                (* top-p (apply u/lcm ps))))
  (weight [_] 1))


(defrecord TimesOp [n pat]
  Op
  (slice [_ start length]
    (-> (slice pat (* start n) (* n length))
        (c/scale (/ n))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord ElongateOp [x pat]
  Op
  (slice [_ start length] (slice pat start length))
  (period [_] (period pat))
  (weight [_] (* x (weight pat))))


(defrecord RepeatOp [n pat]
  Op
  (slice [_ start length] (slice (->TimesOp n pat) start length))
  (period [_] 1)
  (weight [_] (* n (weight pat))))


(defrecord DegradeOp [p pat]
  ;; TODO: Semi Deterministic in case we need to take in multiple slices?
  ;; And if so, how to ensure randomness when desired?
  Op
  (slice [_ start length]
    (map
     (fn [e]
       (if (< (rand) p) e
           (e/assoc-param :init nil)))
     (slice pat start length)))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord MaybeOp [p pat]
  ;; TODO: Too deterministic?
  ;; And if so, how to ensure randomness when desired?
  Op
  (slice [_ start length]
    (let [pat-period (period pat)]
      (map
       (fn [e]
         (let [cycle-num (quot (e/start e) pat-period)
               keep?     (< (u/seeded-rand cycle-num) p)]
           (if keep? e (e/assoc-param e :init nil))))
       (slice pat start length))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord PickOp [pats]
  ;; TODO: Semi Deterministic in case we need to take in multiple slices?
  ;; And if so, how to ensure randomness when desired?
  Op
  (slice [this start length]
    (let [p       (period this)
          i-start (iter-start start p)]
      (->
       (mapcat
        (fn [iter-no]
          (let [lucky (rand-nth pats)]
            (slice lucky iter-no p)))
        (iterate #(+ % p) i-start))
       (c/slice-starts start length))))
  (period [_] (period (first pats)))
  (weight [_] (weight (first pats))))


(comment
  (let [p (->PickOp [(cyc :a :b) (fit :c) (cyc :e :f)])]
    (slice p 1 10)))


(defn bjork
  ([ps os] (bjork ps os []))
  ([ps os res]
   (if (or (not (seq ps)) (not (seq os)))
     (let [step    (concat res ps os)
           [ps os] (split-with #(= (first step) %) step)]
       (if (<= (count os) 1)
         (flatten (concat ps os))
         (recur ps os [])))
     (recur (rest ps) (rest os)
            (conj res (concat (first ps) (first os)))))))


(defrecord EuclidOp [k n r pattern]
  Op
  (slice [_ start length]
    (let [mask            (bjork (repeat k [true]) (repeat (- n k) [nil]))
          mask            (u/rot mask (or r 0))
          children        (map #(and % pattern) mask)]
      (slice
       (->FitOp (map ->pat children))
       start length)))
  (weight [_] 1)
  (period [_] 1))





(comment
  (spin
   (fit [(e/->event :a 1/2 2) (e/->event :a 1/2 2)])))


(defrecord ControlOp [param value-tx pattern]
  Op
  (slice [_ start length]
    (map (fn [e] (e/reassoc-param e :init param value-tx))
         (slice pattern start length)))
  (period [_] (period pattern))
  (weight [_] (weight pattern)))


(defn by-iter
  [pats]
  (group-by (fn [p] (-> p spin first e/iter)) pats))

(defn re-weight
  [pats]
  (let [weights (map weight pats)]
    (if (every? #(= 1 %) weights)
      (mapcat spin pats)
      (mapcat
       (fn [[iter pats]]
         (let [weights          (map weight pats)
               cycls            (map spin pats)
               lengths          (map c/length cycls)
               weighted-lengths (u/weighted lengths weights)
               starts           (reductions + 0 weighted-lengths)]
           (mapcat
            (fn [c s l]
              (c/translate c (+ s iter) l))
            cycls
            starts
            weighted-lengths)))
       (by-iter pats)))))

(defn round-trip
  [op arg cycl mp]
  (let [from (-> cycl first e/start)
        to   (apply max (map e/end cycl))
        pat  (->LitOp (c/translate cycl 0 1) 1 1)
        op   (op arg pat)]
    (-> op
        spin 
        (c/translate from (- to from))
        (->LitOp mp (weight op)))))


(defrecord OpMerge [op arg-pat val-pat]
  Op
  (slice [_ start length]
    (let [arg-cycl (slice arg-pat start length)
          val-cycl (slice val-pat start length)
          mp       (lcp [arg-pat val-pat])]
      (-> (map
           (fn [arg-evt]
             (let [arg     (e/get-init arg-evt)
                   overlap (c/slice-starts val-cycl (e/start arg-evt) (e/length arg-evt))]
               (round-trip op arg overlap mp)))
           arg-cycl)
          re-weight)))
  (period [_] (lcp [arg-pat val-pat]))
  (weight [this] (weight (slice this 0 1))))



;; Option A: 

(comment
  (spin (->OpMerge ->TimesOp (fit 2) (fit :a)))
  (spin (->OpMerge ->TimesOp (fit 1 2) (fit :a :b)))
  (spin (->OpMerge ->TimesOp (cyc 1 2) (fit :a :b)))
  (spin (->OpMerge ->ElongateOp (fit 2 1) (fit :a :b)))
  (spin (->OpMerge ->ElongateOp (cyc (fit 2 1) 1) (fit :a :b)))
  (spin (->OpMerge ->ElongateOp (cyc (fit 2 1) (rep 2 (fit 1 1))) (fit :a :b)))
  (spin (->OpMerge ->ElongateOp (fit 2 1) (fit :a :b)))
  (spin (->OpMerge ->RepeatOp (cyc (el 2 2) 1) (cyc :a :b)))
  (spin (cyc (rep 2 (fit 2 1)) (fit 1 1)))
  (slice (->OpMerge ->TimesOp (cyc 2 1) (fit :a :b)) 1 1)

  (spin
   (x 2 (->LitOp [(->event :b 1/2 1/2)] 1 1)))

  (round-trip ->TimesOp 2 [(->event :b 1/2 1/2)])

  )


(defrecord EventMerge [merge-fn pats]
  Op
  (slice [_ start length]
    (let [cycls (map #(slice % start length) pats)]
      (reduce (fn [merged cycl] (m/merge-cycles merge-fn merged cycl)) cycls)))
  (period [_] (lcp pats))
  (weight [_] (apply max (map weight pats))))


(comment
  (spin* (->EventMerge (m/merge-events-left m/apply-merge) [(fit 2) (fit (partial * 2))])))


(e/realize (u/p + 2) {})



;; ops

(defn fit [& pat]
  (->FitOp (map ->pat pat)))

(defn cyc [& pat]
  (->CyclOp (map ->pat pat)))

(defn x [n & pat]
  (->OpMerge ->TimesOp (->pat n) (->pat pat)))

(defn el [x & pat]
  (->OpMerge ->ElongateOp (->pat x) (->pat pat)))

(defn rep [n & pat]
  (->OpMerge ->RepeatOp (->pat n) (->pat pat)))

(defn deg [p & pat]
  (->OpMerge ->DegradeOp (->pat p) (->pat pat)))

(defn may [p & pat]
  (->OpMerge ->MaybeOp (->pat p) (->pat pat)))

(defn pick [& pats]
  (->PickOp (map ->pat pats)))

(defn n [pat]
  (->ControlOp :n float pat))


(comment
  (spin
   (n (fit 1 2 3))))

(comment
  (spin (fit :a :b :c))
  (spin (fit :a (fit :b :c)))

  (spin (cyc :a :b :c))
  (spin (cyc :a (cyc :b :c)))
  (spin (cyc 1 (cyc :a (cyc :b :c))))

  (spin (cyc :a (fit :b :c)))
  (spin (fit :a (cyc :b :c)))

  (spin (x 2 (fit :a)))
  (spin (x 2 (fit :a :b)))
  (spin (x 2 (fit :a (cyc :b :c))))
  (spin (x 2 (cyc :a :b :c)))
  (spin (x 2 (->LitOp [{:start 1/2 :length 1/4} {:start 3/4 :length 1/4}] 1)))

  (slice (x 2 (fit :a)) 1 2)

  (spin (fit :c (cyc (el 2 :a) :b)))
  (spin (fit :b (x 2 :a)))
  (spin (fit :a (x 2 (cyc :b :c))))
  (spin (fit :b (rep 2 :a)))
  (spin (fit (cyc :a :b) :c))
  (spin (cyc (rep 2 (fit 2 1)) (fit 1 2)))

  (spin (pick (fit :a :b) (fit :c :d) (fit :e :f :g)))

  )


(comment
  (realize (op-merge ->TimesOp (fit 1 2) (fit :a :b)))
  (realize (op-merge ->TimesOp (cyc 1 2) (fit :a :b)) 2)) 
