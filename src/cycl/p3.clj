(ns cycl.p3
  (:require [cycl.event :as e]
            [cycl.util :as u]))

(defn ->event
  ([v] (->event v 0 1))
  ([v start length]
   (let [evt {:start start :length length}]
     (if (map? v)
       (assoc evt :params v)
       (assoc evt :params {:init v})))))

(defprotocol Op
  (slice [this start length])
  (period [this])
  (weight [this]))

(defn weigh
  [pattern]
  (reduce + (map weight pattern)))


(defn cycle-start
  [iter-no period]
  (* period (quot iter-no period)))


(defn event-slice
  [from length evts]
  (->> evts
       (drop-while #(< (e/start %) from))
       (take-while #(< (e/start %) (+ from length)))))


(defn spin
  [pat]
  (let [p (period pat)]
    (slice pat 0 p)))


(defrecord Pure [value]
  Op
  (slice [_ start length]
    (let [start (long (Math/ceil start))]
      (->> (map #(->event value % 1) (iterate inc start))
           (event-slice start length))))
  (period [_] 1)
  (weight [_] 1))

(comment
  (let [v (->Pure :a)]
    (slice v 1/2 1/3)))

(defn lcp
  [pats]
  (if (= 1 (count pats))
    (period (first pats))
    (apply u/lcm (map period pats))))

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
    (let [cyc-start (cycle-start (long start) (period this))]
      (->>
       (arrange
        (fn [evt offset weight n]
          (let [[_ frac] (u/mixed (e/start evt))
                scale    (/ n)]
            (-> evt
                (assoc :start (+ cyc-start (* offset scale) (* frac scale weight)))
                (update :length #(* % scale weight)))))
        patterns)
       (event-slice start length))))
  (period [_] (lcp patterns))
  (weight [_] 1))

(defrecord CyclOp [patterns]
  Op
  (slice [this start length]
    (let [c-start (cycle-start (long start) (period this))]
      (->>
       (arrange
        (fn [evt offset weight n]
          (let [[_ frac] (u/mixed (e/start evt))]
            (-> evt
                (assoc :start (+ c-start offset (* frac weight)))
                (update :length #(* % weight)))))
        patterns)
       (event-slice start length))))

  (period [_] (let [ps    (map period patterns)
                    top-p (weigh patterns)]
                (* top-p (apply u/lcm ps))))
  (weight [_] 1))

(defrecord TimesOp [n pat]
  Op
  (slice [_ start length]
    (let [events  (slice pat start length)
          c-start (-> events first e/start)]
      (->>
       (mapcat
        (fn [cycl offset]
          (for [evt cycl]
            (-> evt
                (update :start #(+ offset (/ (- % c-start) n)))
                (update :length #(/ % n)))))
        (repeat n events)
        (iterate #(+ % (/ length n)) c-start))
       (event-slice start length))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord TimesOp [n pat]
  Op
  (slice [_ start length]
    (map (fn [e]
           (-> e
               (update :start #(/ % n))
               (update :length #(/ % n))))
         (slice pat (* start n) (* n length))))
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
               keep? (< (u/seeded-rand cycle-num) p)]
           (if keep? e (e/assoc-param e :init nil))))
       (slice pat start length))))
  (period [_] (period pat))
  (weight [_] (weight pat)))

(defrecord PickOp [pats]
  ;; TODO: Semi Deterministic in case we need to take in multiple slices?
  ;; And if so, how to ensure randomness when desired?
  Op
  (slice [this start length]
    (let [p (period this)
          c-start (cycle-start start p)]
      (->>
       (mapcat
        (fn [iter-no]
          (let [lucky (rand-nth pats)]
            (slice lucky iter-no p)))
        (iterate #(+ % p) c-start))
       (event-slice start length))))
  (period [_] (period (first pats)))
  (weight [_] (weight (first pats))))


(comment
  (let [p (->PickOp [(cyc :a :b) (fit :c) (cyc :e :f)])]
    (slice p 1 10)))


(defrecord LitOp [events p w]
  Op
  (slice [this start length]
    (let [c-start (cycle-start start (period this))]
      (->>
       (mapcat
        (fn [cycl iter]
          (for [evt cycl]
            (-> evt (update :start #(+ % iter)))))
        (repeat events)
        (iterate #(+ % p) c-start))
       (event-slice start length))))
  (period [_] p)
  (weight [_] w))


(comment
  (spin
   (fit [(->event :a 1/2 2) (->event :a 1/2 2)])))


(defrecord ControlOp [param value-tx pattern]
  Op
  (slice [_ start length]
    (map (fn [e] (e/reassoc-param e :init param value-tx))
         (slice pattern start length)))
  (period [_] (period pattern))
  (weight [_] (weight pattern)))


(defn c-start
  [cycl]
  (-> cycl first e/start))

(defn c-end
  [cycl]
  (apply max (map e/end cycl)))

(defn c-length
  [cycl]
  (- (c-end cycl) (c-start cycl)))

(defn translate
  [cycl beg len]
  (let [orig-start (-> cycl first e/start)
        orig-end   (apply max (map e/end cycl))
        orig-len   (- orig-end orig-start)
        factor     (/ len orig-len)]
    (->
     (for [evt cycl]
       (-> evt
           (update :start #(+ beg (* (- % orig-start) factor)))
           (update :length #(* % factor)))))))

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
               lengths          (map c-length cycls)
               weighted-lengths (u/weighted lengths weights)
               starts           (reductions + 0 weighted-lengths)]
           (mapcat
            (fn [c s l]
              (translate c (+ s iter) l))
            cycls
            starts
            weighted-lengths)))
       (by-iter pats)))))

(defn round-trip
  [op arg cycl mp]
  (let [from (-> cycl first e/start)
        to   (apply max (map e/end cycl))
        pat (->LitOp (translate cycl 0 1) 1 1)
        op (op arg pat)]
    (-> op
        spin 
        (translate from (- to from))
        (->LitOp mp (weight op)))))


(defrecord MergeOp [op arg-pat val-pat]
  Op
  (slice [_ start length]
    (let [arg-cycl            (slice arg-pat start length)
          val-cycl            (slice val-pat start length)
          mp                  (lcp [arg-pat val-pat])
          merged
          (map
           (fn [arg-evt]
             (let [arg     (e/get-init arg-evt)
                   overlap (event-slice (e/start arg-evt) (e/length arg-evt) val-cycl)]
               (round-trip op arg overlap mp)))
           arg-cycl)]
      (re-weight merged)))
  (period [_] (lcp [arg-pat val-pat]))
  (weight [this] (weight (slice this 0 1))))


;; Option A: 

(comment
  (spin (->MergeOp ->TimesOp (fit 2) (fit :a)))
  (spin (->MergeOp ->TimesOp (fit 1 2) (fit :a :b)))
  (spin (->MergeOp ->TimesOp (cyc 1 2) (fit :a :b)))
  (spin (->MergeOp ->ElongateOp (fit 2 1) (fit :a :b)))
  (spin (->MergeOp ->ElongateOp (cyc (fit 2 1) 1) (fit :a :b)))
  (spin (->MergeOp ->ElongateOp (cyc (fit 2 1) (rep 2 (fit 1 1))) (fit :a :b)))
  (spin (->MergeOp ->ElongateOp (fit 2 1) (fit :a :b)))
  (spin (->MergeOp ->RepeatOp (cyc (el 2 2) 1) (cyc :a :b)))
  (spin (cyc (rep 2 (fit 2 1)) (fit 1 1)))
  (slice (->MergeOp ->TimesOp (cyc 2 1) (fit :a :b)) 1 1)

  (spin
   (x 2 (->LitOp [(->event :b 1/2 1/2)] 1 1)))

  (round-trip ->TimesOp 2 [(->event :b 1/2 1/2)])

  )


(defn ->pat [x]
  (cond
    (satisfies? Op x) x
    (sequential? x)  (cond
                       (c/cycl? x) (->LitOp x)
                       :else (->FitOp (map ->pat x)))
    :else             (->Pure x)))

;; ops

(defn fit [& pat]
  (->FitOp (map ->pat pat)))

(defn cyc [& pat]
  (->CyclOp (map ->pat pat)))

(defn x [n & pat]
  (->TimesOp n (->pat pat)))

(defn el [x & pat]
  (->ElongateOp x (->pat pat)))

(defn rep [n & pat]
  (->RepeatOp n (->pat pat)))

(defn deg [p & pat]
  (->DegradeOp p (->pat pat)))

(defn may [p & pat]
  (->MaybeOp p (->pat pat)))

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


(spin
 (fit :bd (x 2 (cyc :sd :hh))))


(slice
 (x 2 (cyc :sd :hh))
 1/2 1)


(spin
 (fit (->LitOp [(->event :a 1 1)] 2) #_(->LitOp [(->event nil 0 1) (->event :b 1 1)] 2)))
