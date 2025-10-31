(ns scratch
  {:clj-kondo/ignore true}
  (:require
   [cycl.pattern :as p]
   [cycl.events :as e]
   [cycl.merge :as m]
   [cycl.util :refer [toggle!] :as u]
   [cycl.ing :as c :refer [start! shutdown! o once sh! pause! now!]]
   [cycl.ops :refer :all]
   [cycl.dirt :refer [connect-dirt]]
   [clojure.pprint :refer [print-table]]
   [overtone.at-at :refer [now]]
   [cycl.music :as mu]))


(reset! c/defaults {:target :default})
(c/set-cps! 1/2)

(c/now! (cyc {:fn #(println "[ONE]")} {:fn #(println "[TWO]")}))

(c/now! (cyc (f #(println "ONE") #(println "TWO"))))
