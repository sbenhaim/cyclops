(ns shhh.util)

(defn cycle-n
  [n seq]
  (let [len (count seq)
        n* (* n len)]
    (take n* (cycle seq))))


(defn toggle! [a]
  (swap! a not))
