(ns cyclops.util)

(defn cycle-n
  [n seq]
  (let [len (count seq)
        n* (* n len)]
    (take n* (cycle seq))))


(defn toggle! [a]
  (swap! a not))


(defn rot [s n]
  (take (count s) (drop n (cycle s))))
