(ns cyclops.play
  {:clj-kondo/ignore true}
  (:require
   [cyclops.pattern :as p]
   [cyclops.events :as e :refer [events]]
   [cyclops.music :as m :refer [chord scale]]
   [cyclops.util :refer [toggle!] :as u]
   [cyclops.core :refer [o once sh! pause!] :as c]
   [cyclops.ops :refer :all]
   [clojure.pprint :refer [print-table]]
   [overtone.at-at :refer [now]]))


;; TODO: Is there an elegant way to apply pattern fns (slow/x) to a cycle?
;; TODO: How to accept patterns as arguments to pattern fns?
;; (slow [1 2 3] [:a :b :c]) == [(slow 1 :a) (slow 2 :b) (slow 3 :c)]
;; (slow [1 2 [3 4]] (n :a :b :c))
;; We'll need to see how tidal/strudel do it. Is this even something to support?
;; Can pattern implement cyclic? Can cycles be events? Would they just need to implement realize-event?
;; Realize could be polymorphic and call realize-event on events and realize-cycle on cycles, maybe.
;; But a cycle has timing info which would need to adapt to event context.
;; Though we do have events sending in context. Maybe it's just more offsets?

(comment ;api
  (o 1 :a :b :c) ; Not this
  ; or
  (o 1 (n :a :b :c) (s :supersaw))
  (->> [:a :b :c] n (o 1 (s :supersaw)))

  (o 1 (n (sin 50 100 3)) (s :supersaw))

  (o 1 (s :bd :sd :sd) (x [3 2 3])) ; => "bd!3 sd!2 sd!3" => ctrl + single arg representing params is a partial

  ; or we just use threading
  (o 1 (->> (s :bd :sd :sd) (x [3 2 3])))  (->> [:bd :sd :sd] s (x [3 2 3]) (o 1))
  ; NOTE: This one

  ; Do I support
  (+| [1 2 3] [4 5 6]) ; <= Requires either that seqs/patterns implement Cyclic or
                                        ; that merge be polymorphic and support patterns
                                        ; or that we use pattern specific merge conditionals (probably a bad idea)
                                        ; or that we somehow convert patterns to cycles somewhere
                                        ; Maybe not crazy that patterns can become cycles?
                                        ; Advantage: Create a general pattern and then assign it to a ctrl
                                        ; Stretch goal

  (+| (n 1 2 3) [4 5 6]) ; <= easy to type, requires some hidden assumptions,
  ; like that unassigned values merge with any or certain or the first existing
  ; param? Frankly don't like it
  ; => #Event{:n 1 :init 4}

  ; or require
  (+| (n 1 2 3) (n 4 5 6)) ; <= simple and clear and easier to implement
  ; This one plus stretch goal
  )

;; (mixer/boot-server-and-mixer)
(c/start!)
(c/shutdown!)

(once (n (range 60 64)) (s :superpiano))

(once
 (s|
  (+| (s :superpiano) (n (range 60 64)) (pan 0))
  (+| (s :superpiano) (rev-cycl (n (range 60 64))) (pan 1))))


(evts
 (n (->> [1 2 3] (slow 2))))


(evts
 (rev (s :superpiano)))

evts
(+| (s :superpiano) (n (range 60 64)) (pan 1))

(evts
 (s| (+| (n 10) (s :supermandolin) (pan 0)) (+| (n 20) (s :superpiano) (pan 1))))

(events
 (+| (range 10) (range 10)))


(pause!)
(->> (x 2 :bd :hh :sd :hh)  s once)

(e/events
 (f| Math/round
     (n 2.5 3.2 4.1)
     #_[(fn [_ v] (Math/round v))]))

(->> [2.5 3.2 4.1] (map inc) n events)

(map e/realize
     (events
      (n (fn [v _] (Math/floor v)))
      true))

(toggle! c/verbose)
(toggle! c/debug-osc)
(c/init-client!)
(c/shutdown!)

(once
 (n 60)
 (n 63)
 (n 67)
 (s :supermandolin))


(e/events
 (f| vector
     (n 60)
     (n 70)
     (n 63)
     (s :supermandolin)))


(o 0 (s (slow 3 (rep 8 :supermandolin ))) (n (e/sin 60 72 3))) ;; TODO: Why does this stutter?
(c/start!)
(pause!)


(once (n (mu/cycle-chord :cm7)) (s :supermandolin))

(sh!)
(speak!)

(cyc|ops)

(e/events (+| (n (slow* 2 (rep* 16 (e/sin 60 72)))) (s* :supersaw)))

(p/process-pattern (slow 2 (rep 16 (e/sin 60 72))))

(c/send-dirt {:s :supermandolin :n 66})


(o 0 (|+| (n* (e/sin 60 70) (e/sin 60 70) (e/sin 60 70) (e/sin 60 70)) (s* :supersaw)))


(e/slice
 (|+| (n* (e/sin 60 70) (e/sin 60 70) (e/sin 60 70) (e/sin 60 70)) (s* :supersaw) (legato* 1))
 (e/tc 0 1)
 {:realize? true})

(p/process-pattern
 (rep 4 [(e/sin 60 70)]))


(c/start!)
(c/pause!)

(toggle! c/verbose)
(toggle! c/sh)

(c/continue!)
(c/pause!)

(@c/layers 0)

(c/start!)
(c/sh!)
(c/speak!)

(reset! c/cps 1) ;; TODO: Why does changing this break stuff?

(o 0 (n (x 2 :c :a :f :e)) (s :superpiano) (pan (e/sin)))
(c/shutdown!)


(o 1
 (s (slow 2 (rep 8 :bd)))
 (pan (slow 2 (rep 8 0 1))))

(e/slice
 (pan
  (e/sin 0 1 1))
 0 1 {:realize? true})


(e/cycle-events 1 (p/process-pattern [(e/sin 0 1 1)]))



(once
 (|s|  (n :c) (n :e) (n :g) (s :supermandolin) (s :supersaw)))


(e/events
 (|s| (n :c) (n :e) (n :g) (s :supersaw) (s :supermandolin))
 true)


(e/slice )

(once (->> (n (chord :cm7 :o 3 :incl 1)) (juxt rev)) (s :supermandolin))

(once (+| (n :c2 :eb3 :g3 [:bb3 :c4])) (s :superpiano))

(slow 0.5)
