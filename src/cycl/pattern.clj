(ns cycl.pattern
  (:require [cycl.event :as e]
            [cycl.val :as v]
            [cycl.util :as u]
            [cycl.cycl :as c]
            [cycl.merge :as m]))


(defprotocol Pattern
  (gen [this start length])
  (period [this])
  (weight [this]))


(defn weigh
  [pats]
  (reduce + (map weight pats)))


(defn iter-start
  [cycle-no period]
  (* period (quot cycle-no period)))


(defn spin
  [pat]
  (let [p (period pat)]
    (gen pat 0 p)))


(defn lcp
  [pats]
  (case (count pats)
    0 1
    1 (period (first pats))
    (apply u/lcm (map period pats))))


(defrecord Pure [value]
  Pattern
  (gen [_ start length]
    (let [i-start (long (Math/floor start))]
      (-> (map #(e/->event value % 1) (iterate inc i-start))
          (c/slice start length))))
  (period [_] 1)
  (weight [_] 1))


(comment
  (gen (->Pure :a) 1/2 2))


(defrecord Lit [events p w]
  Pattern
  (gen [this start length]
    (let [i-start (iter-start start (period this))]
      (->
       (mapcat
        (fn [cycl iter]
          (for [evt cycl]
            (-> evt (update :start #(+ % iter)))))
        (repeat events)
        (iterate #(+ % p) i-start))
       (c/slice start length))))
  (period [_] p)
  (weight [_] w))


(defn arrange
  ;; TODO: Calculating inf seq and then take-while always creates extra event
  [tx-evt pats]
  (let [weights  (map weight pats)
        n        (reduce + weights)
        weights* (cycle weights)
        offsets* (reductions + 0 weights*)]
    (-> (mapcat
         (fn [pat weight offset]
           (let [sub-iter-no (quot offset n)
                 sub-pat     (gen pat sub-iter-no 1)]
             (for [evt sub-pat]
               (tx-evt evt offset weight n))))
         (cycle pats)
         weights*
         offsets*))))


(defrecord Fit [pats]
  Pattern
  (gen [this start length]
    (if (seq pats)
      (let [i-start (iter-start (long start) (period this))]
        (->
         (arrange
          (fn [evt offset weight n]
            (let [[_ frac] (u/mixed (e/start evt))
                  scale    (/ n)]
              (-> evt
                  (assoc :start (+ i-start (* offset scale) (* frac scale weight)))
                  (update :length #(* % scale weight)))))
          pats)
         (c/slice start length)))
      []))
  (period [_] (lcp pats))
  (weight [_] 1))



(comment
  (gen (->Fit [(->pat :a)]) 1 3/2))


(defrecord Speed [x pat]
  Pattern
  (gen [_ start length]
    (-> (gen pat (* start x) (* length x))
        (c/scale (/ x))
        (->> (map (fn [e] (e/update-param e :period #(* (or % 1) (/ x))))))))
  (period [_] (/ (period pat) x))
  (weight [_] (weight pat)))


(comment
  (gen
   (->Speed 1/3 (->Pure :a))
   1 2))


(defn ->pat [x]
  (cond
    (satisfies? Pattern x) x
    (sequential? x)        (if (c/cycl? x)
                             (->Lit x 1 1)
                             (->Fit (map ->pat x)))
    (nil? x)               (->Fit [])
    :else                  (->Pure x)))



(defrecord Cyc [pats]
  Pattern
  (gen [this start length]
    (if (seq pats)
      (let [i-start (iter-start (long start) (period this))]
        (->
         (arrange
          (fn [evt offset weight n]
            (let [[_ frac] (u/mixed (e/start evt))]
              (-> evt
                  (assoc :start (+ i-start offset (* frac weight)))
                  (update :length #(* % weight)))))
          pats)
         (c/slice start length)))
      []))
  (period [_] (let [top-p (weigh pats)]
                (* top-p (lcp pats))))
  (weight [_] 1))


(defrecord Times [n pat]
  Pattern
  (gen [_ start length]
    (-> (gen pat (* start n) (* n length))
        (c/scale (/ n))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(comment
  (gen (->Times 2 (->Pure :a)) 0 1))


(defrecord Elongate [x pat]
  Pattern
  (gen [_ start length] (gen pat start length))
  (period [_] (period pat))
  (weight [_] (* x (weight pat))))


(defrecord Repeat [n pat]
  Pattern
  (gen [_ start length] (gen (->Times n pat) start length))
  (period [_] 1)
  (weight [_] (* n (weight pat))))


(defrecord Splice [pats]
  Pattern
  (gen [_ start length] (gen (->Fit pats) start length))
  (period [_] 1) ;; ?
  (weight [_] (weigh pats)))


(defrecord Degrade [p pat]
  ;; TODO: Semi Deterministic in case we need to take in multiple slices?
  ;; And if so, how to ensure randomness when desired?
  Pattern
  (gen [_ start length]
    (map
     (fn [e]
       (if (< (rand) p) e
           (e/assoc-param :init nil)))
     (gen pat start length)))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord Maybe [p pat]
  ;; TODO: Too deterministic?
  ;; And if so, how to ensure randomness when desired?
  Pattern
  (gen [_ start length]
    (let [pat-period (period pat)]
      (map
       (fn [e]
         (let [cycle-num (quot (e/start e) pat-period)
               keep?     (< (u/seeded-rand cycle-num) p)]
           (if keep? e (e/assoc-param e :init nil))))
       (gen pat start length))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord Pick [pats]
  ;; TODO: Semi Deterministic in case we need to take in multiple slices?
  ;; And if so, how to ensure randomness when desired?
  Pattern
  (gen [this start length]
    (let [p       (period this)
          i-start (iter-start start p)]
      (->
       (mapcat
        (fn [iter-no]
          (let [lucky (rand-nth pats)]
            (gen lucky iter-no p)))
        (iterate #(+ % p) i-start))
       (c/slice start length))))
  (period [_] (period (first pats)))
  (weight [_] (weight (first pats))))


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


(defrecord Euclid [k n r pat]
  Pattern
  (gen [_ start length]
    (let [mask            (bjork (repeat k [true]) (repeat (- n k) [nil]))
          mask            (u/rot mask (or r 0))
          children        (map #(and % pat) mask)]
      (gen
       (->Fit (map ->pat children))
       start length)))
  (weight [_] 1)
  (period [_] 1))


(defrecord Reverse [pat]
  Pattern
  (gen [this start length]
    (let [p    (period this)
          evts (gen pat start length)]
      (-> (map
           (fn [evt]
             (let [evt-start  (e/start evt)
                   evt-length (e/length evt)
                   ;; Find which cycle this event belongs to
                   cycle-num  (long (Math/floor (/ evt-start p)))
                   cycle-base (* cycle-num p)
                   ;; Mirror position within the cycle
                   rel-start  (- evt-start cycle-base)
                   rel-end    (+ rel-start evt-length)
                   new-rel    (- p rel-end)
                   new-start  (+ cycle-base new-rel)]
               (assoc evt :start new-start)))
           evts)
          (c/sort-cycl)
          (c/slice start length))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord Control [param value-tx pat]
  Pattern
  (gen [_ start length]
    (map (fn [e] (e/reassoc-param e :init param #(v/->Realize+Apply value-tx %)))
         (gen pat start length)))
  (period [_] (period pat))
  (weight [_] (weight pat)))


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
  (let [start (c/start cycl)
        len   (c/length cycl)
        pat  (->Lit (c/translate cycl 0 1) 1 1)
        op   (op arg pat)]
    (-> op
        spin 
        (c/translate start len)
        (->Lit mp (weight op)))))


(defrecord OpMerge [op arg-pat val-pat]
  ;; TODO: Could this be simpler?
  Pattern
  (gen [_ start length]
    (let [arg-cycl (gen arg-pat start length)
          val-cycl (gen val-pat start length)
          mp       (long (Math/ceil (+ start length)))]
      (-> (map
           (fn [arg-evt]
             (let [arg     (e/get-init arg-evt)
                   overlap (c/slice val-cycl (e/start arg-evt) (e/length arg-evt))
                   overlap (filter :trigger? overlap)]
               (round-trip op arg overlap mp)))
           arg-cycl)
          re-weight)))
  (period [_] (lcp [arg-pat val-pat]))
  (weight [_] (let [args (spin arg-pat)]
                (if (= 1 (count args))
                  (weight
                   (op (-> args first e/get-init) val-pat))
                  1))))


(defrecord EventMergeLeft [merge-fn pats]
  Pattern
  (gen [_ start length]
    (if (seq pats) 
      (let [cycls (map #(gen % start length) pats)]
        (reduce (fn [merged cycl] (m/merge-cycles-left merge-fn merged cycl)) cycls))
      []))
  (period [_] (lcp pats))
  (weight [_] (apply max (map weight pats))))


(defrecord EventMergeSplit [merge-fn pats]
  Pattern
  (gen [_ start length]
    (if (seq pats) 
      (->
       (let [cycls (map #(gen % start length) pats)]
         (reduce (fn [merged cycl] (m/merge-cycles-split merge-fn merged cycl)) cycls))
       (c/slice start length))
      []))
  (period [_] (lcp pats))
  (weight [_] (apply max (map weight pats))))



(defrecord Stack [pats]
  Pattern
  (gen [_ start length]
    (->> pats
         (mapcat #(gen % start length))
         (c/sort-cycl)))
  (period [_] (lcp pats))
  (weight [_] 1))


(defrecord Chop [n pat]
  Pattern
  (gen [_ start length]
    (let [evts (gen pat start length)]
      (mapcat
       (fn [evt]
         (let [evt-start  (e/start evt)
               evt-length (e/length evt)
               ;; Get existing begin/end or default to 0-1
               begin      (or (e/get-param evt :begin) 0)
               end        (or (e/get-param evt :end) 1)
               rng        (- end begin)]
           ;; Create n slices, each with proportional begin/end
           (for [i (range n)]
             (let [slice-begin  (+ begin (* rng (/ i n)))
                   slice-end    (+ begin (* rng (/ (inc i) n)))
                   ;; Each slice takes 1/n of the original event's time
                   slice-start  (+ evt-start (* evt-length (/ i n)))
                   slice-length (/ evt-length n)]
               (-> evt
                   (assoc :start slice-start)
                   (assoc :length slice-length)
                   (e/assoc-param :begin slice-begin)
                   (e/assoc-param :end slice-end))))))
       evts)))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord Striate [n pat]
  Pattern
  (gen [_ start length]
    (let [inner-p (period pat)
          evts    (gen pat start length)]
      (map
       (fn [evt]
         (let [evt-start   (e/start evt)
               cycle-num   (long (Math/floor (/ evt-start inner-p)))
               slice-idx   (mod cycle-num n)
               begin       (or (e/get-param evt :begin) 0)
               end         (or (e/get-param evt :end) 1)
               rng         (- end begin)
               slice-begin (+ begin (* rng (/ slice-idx n)))
               slice-end   (+ begin (* rng (/ (inc slice-idx) n)))]
           (-> evt
               (e/assoc-param :begin slice-begin)
               (e/assoc-param :end slice-end))))
       evts)))
  (period [_] (* n (period pat)))
  (weight [_] (weight pat)))


(defrecord StriatBy [n len pat]
  Pattern
  (gen [_ start length]
    (let [inner-p (period pat)
          evts    (gen pat start length)]
      (map
       (fn [evt]
         (let [evt-start   (e/start evt)
               cycle-num   (long (Math/floor (/ evt-start inner-p)))
               slice-idx   (mod cycle-num n)
               begin       (or (e/get-param evt :begin) 0)
               end         (or (e/get-param evt :end) 1)
               rng         (- end begin)
               slice-begin (+ begin (* rng (/ slice-idx n)))
               slice-end   (min (+ slice-begin (* rng len)) end)]
           (-> evt
               (e/assoc-param :begin slice-begin)
               (e/assoc-param :end slice-end))))
       evts)))
  (period [_] (* n (period pat)))
  (weight [_] (weight pat)))

