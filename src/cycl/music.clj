(ns cycl.music
  (:require [overtone.music.pitch :as m]))


(defn note-like?
  [sym]
  (re-find #"^([a-gA-G][bB]?[#]?)(\d*)$" (name sym)))


(defn parse-note [sym]
  (let [note-match #"([a-gA-G][bB]?[#]?)(\d*)"
        [orig note octave] (re-find note-match (name sym))]
    (when orig
      {:note note
       :octave (-> octave str parse-long)})))

(defn note
  ([n]
   (let [n      (name n)
         o (re-find #"(.*?)(\d)" n)]
     (if o (note (second o) (-> o last parse-long))
        (note n 4))))
  ([n o]
   (m/note (str (name n) (or o 4)))))


(defn parse-chord [sym]
  (let [chord-match #"([a-gA-G][bB]?[#]?)(\d)?:?(.+)"
        [orig note-name octave chord] (re-find chord-match (name sym))
        octave (-> octave str parse-long)]
    (when orig
      {:note-name note-name
       :octave octave
       :midi (note note-name octave)
       :chord (keyword chord)})))


(comment
  (parse-note :c#7)
  (parse-chord :c5:5)
  (parse-chord :c3m7))


(defn cycle+12
  [col]
  (assert (coll? col))
  (->> col
       repeat
       (mapcat
        (fn [i notes]
          (for [n notes]
            (+ n (* i 12))))
        (range))))


(defn chord
  [sym & {:keys [o n incl] :or {incl 0
                                o 1}}]
  (let [{:keys [midi chord]} (parse-chord sym)
        notes (m/chord midi chord)]
    (if (seq notes)
      (let [cyc (cycle+12 notes)]
        (cond
          n     (take n cyc)
          o     (take (+ incl (* o (count notes))) cyc)
          :else cyc))
      (throw (ex-info "Chord not found" {:chord-name sym})))))


(defn chord-nth
  [sym]
  (fn [v]
    (let [{:keys [midi chord]} (parse-chord sym)
          notes                (m/chord midi chord)]
      (if (seq notes)
        (nth (cycle+12 notes) (or v 0))
        (throw (ex-info "Chord not found" {:chord-name sym}))))))

;; TODO: Expose shorted form in ops?

(defn -scale
  [root scale-name]
  (m/scale (note root) (keyword scale-name)))

(defn scale
  [root scale-name & {:keys [o n incl]
                      :or   {incl 0
                             o    1}}]
  (let [notes (butlast (-scale root scale-name))]
    (if (seq notes)
      (let [cyc (cycle+12 notes)]
        (cond
          n     (take n cyc)
          o     (take (+ incl (* o (count notes))) cyc)
          :else cyc))
      (throw (ex-info "Scale not found" {:scale-name scale-name})))))


(defn scale-nth
  [root scale-name]
  (fn [v]
    (let [notes (butlast (-scale root scale-name))]
      (if (seq notes)
        (nth (cycle+12 notes) (or v 0))
        (throw (ex-info "Scale not found" {:scale-name scale-name}))))))
