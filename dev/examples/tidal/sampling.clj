(ns examples.tidal.sampling
  (:require
   [cycl.dirt]
   [cycl.core :as c :refer [start! shutdown! o once sh! pause! now!]]
   [cycl.ops :refer :all]))

;; d1 $ chop 16 $ sound "arpy ~ feel*2 newnotes"


(now! (s :arpy :- (x 2 :feel) :newnotes))

(now! (chop 16) )

;; d1 $ chop "<16 128 32>" $ sound "arpy ~ feel*2 newnotes"


;; d1 $ slow 2 $ rev $ chop 16 $ sound "breaks125"


;; d1 $ slow 2 $ jux rev $ chop 16 $ sound "breaks125"


;; d1 $ chop 16 $ sound (samples "arpy*8" (run 16))
;; d1 $ chop 32 $ sound (samples "arpy*8" (run 16))
;; d1 $ chop 256 $ sound "bd*4 [sn cp] [hh future]*2 [cp feel]"


;; d1 $ loopAt 8 $ chop 32 $ sound "bev"


;; d1 $ loopAt 8 $ rev $ chop 32 $ sound "bev"

;; d1 $ slow 4 $ striate 3 $ sound "numbers:0 numbers:1 numbers:2 numbers:3"

;; d1 $ slow 4 $ chop 3 $ sound "numbers:0 numbers:1 numbers:2 numbers:3"

;; d1 $ slow 4 $ sound "numbers:0 numbers:1 numbers:2 numbers:3"

;; d1 $ slow 32 $ striateBy 32 (1/16) $ sound "bev"

;; d1 $ slice 8 "7 6 5 4 3 2 1 0" $ sound "breaks165"
  ;; # legato 1

;; d1 $ slice 8 "[<0*8 0*2> 3*4 2 4] [4 .. 7]" $ sound "breaks165"
;;   # legato 1

;; d1 $ splice 8 "[<0*8 0*2> 3*4 2 4] [4 .. 7]" $ sound "breaks165"

;; d1 $ randslice 32 $ sound "bev"

;; d1 $ fast 4 $ randslice 32 $ sound "bev"

;; d1 $ bite 4 "0 1*2 2*2 [~ 3]" $ n "0 .. 7" # sound "drum"

;; d1 $ chew 4 "0 1*2 2*2 [~ 3]" $ n "0 .. 7" # sound "drum"

;; d1 $ loopAt 4 $ sound "breaks125"

;; d1 $ loopAt 4 $ chop 32 $ sound "breaks125"

;; d1 $ juxBy 0.6 (|* speed "2") $ loopAt "<4 6 2 3>" $ chop 12 $ sound "fm:14"

;; d1 $ smash 3 [2,3,4] $ sound "ho ho:2 ho:3 hc"

;; d1 $ slow "<2 3 4>" $ striate 3 $ sound "ho ho:2 ho:3 hc"

;; d1 $ smash 6 [2,3,4] $ sound "ho ho:2 ho:3 hc"

;; d1 $ smash' 6 [2,3,4] $ sound "ho ho:2 ho:3 hc"

;; d1 $ smash 12 [2,3,4] $ s "bev*4"

;; d1 $ smash' 12 [2,3,4] $ s "bev*4"

;; d1 $ n (slow 2 $ segment 16 $ range 0 32 $ sine) # sound "amencutup"

;; saw = sig $ \t -> mod' (fromRational t) 1
