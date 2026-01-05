(ns cycl.val)


(defprotocol DoYouRealize?
  (realize [this ctx]))


(extend-protocol DoYouRealize?
  nil
  (realize [_ _] nil)

  java.lang.Object
  (realize [o _] o))


(defrecord Realize+Apply [b a]
  DoYouRealize?
  (realize [_ ctx]
    (let [a* (realize a ctx)]
      (realize (b a*) ctx))))


(defn get-param
  ([ctx k] (get-param ctx k nil))
  ([ctx k default]
   (let [v (get-in ctx [:params k] default)]
     v))) ;; TODO: (realize v ctx)?


(defrecord RandVal [ampl]
  DoYouRealize?
  (realize [_ ctx]
    (rand (or ampl (get-param ctx :ampl 1)))))

;; Naming convention
;; Takes a val
;; sina

(defn randn [ampl]
  (->RandVal ampl))

(def rand1 (randn 1))

(def rand* (randn nil))

(realize (randn 10) {})

;; TODO: Fn for gettting realize value from context?

(defrecord TrigVal [trig-fn period ampl]
  DoYouRealize?
  (realize [_ ctx]
    (let [period (or period (get-param ctx :period 1))
          ampl   (or ampl (get-param ctx :ampl 1))
          pos    (:start ctx)]
      (-> pos           ;; What part of the cycl are we on
          (* 2 Math/PI) ;; Maths
          (/ period)    ;; How many cycles to stretch over
          trig-fn       ;; Trig
          (+ 1)         ;; -1 to 1 => 0 to 2
          (* ampl)      ;; Amplitude
          (/ 2)))))


(defn sin [& {:keys [p a]}]
  (->TrigVal Math/sin p a))

(def sin1 (sin :p 1 :a 1))

(def sin* (sin))

(defn cos [& {:keys [p a]}]
  (->TrigVal Math/cos p a))



(defn amp
  ([max] (amp 0 max))
  ([min max]
   (fn [v]
     (let [mult (-> max (- min))]
       (+ min (* mult (or v 0)))))))


(comment
  (defn sin [ampl {:keys [event]}]
    (trig-fn Math/sin (:start event) 1 #_(:period event) (or ampl 1)))


  (defn cos [ampl {:keys [event]}]
    (trig-fn Math/cos (:start event) 1 #_(:period event) (or ampl 1))))


(defn square [ampl {:keys [event]}]
  (let [half (/ (:period event) 2)]
       (if (< (:start event) half)
         0
         (or ampl 1))))


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





(defn irand
  [ampl]
  #(rand-int ampl))
