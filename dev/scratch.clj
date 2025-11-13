(ns scratch
  {:clj-kondo/ignore true}
  (:require
   [cycl.pattern :as p]
   [cycl.events :as e]
   [cycl.merge :as m]
   [cycl.util :refer [toggle!] :as u]
   [cycl.ing :as c :refer [start! shutdown! o once sh! pause! now! ->evts]]
   [cycl.ops :refer :all]
   [cycl.dirt :refer [connect-dirt]]
   [clojure.pprint :refer [print-table]]
   [overtone.at-at :refer [now]]
   [cycl.music :as mu]))


(reset! c/defaults {:target :default})
(c/set-cps! 1)

(c/now! (cyc {:fn #(println "[ONE]")} {:fn #(println "[TWO]")}))

(c/now! (cyc (f #(println "ONE") #(println "TWO"))))


(evts
 (+| (chop 2) (s :arpy)))

(now! (chop 10) (s :arpy))

;; d1 $ slow 2 $ rev $ chop 16 $ sound "breaks125"


(now! (->> 16 chop rev (slow 2)) (s "breaks125"))

(start!)
(o 1 (chop (cyc 16 128 32)) (s :arpy :- (x 2 :feel) :newnotes))
(o 1 (s :arpy :- (x 2 :feel) :newnotes) (chop 64))
(o 1 (chop 16) (s :arpy :arpy))
(o l)
