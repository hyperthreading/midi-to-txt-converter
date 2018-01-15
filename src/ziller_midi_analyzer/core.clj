(ns ziller-midi-analyzer.core
  (:require [clojure.java.io :as io])
  (:import [javax.sound.midi
            Sequence
            Track
            MidiEvent
            MidiMessage
            ShortMessage
            MetaMessage
            MidiSystem])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def note-names ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])
(def note-on 0x90)
(def note-off 0x80)

(defn midi-events-on-track
  [track]
  (let [n (.size track)]
    (map #(let [event (.get track %)
                tick  (.getTick event)
                msg   (.getMessage event)]
            {:event event
             :tick  tick
             :msg   msg})
         (range n))))

(defn note->str
  [msg]
  (let [key       (.getData1 msg)
        cmd       (.getCommand msg)
        note      (int (mod key 12))
        note-name (nth note-names note)]
    (cond 
      (= cmd note-on)  (str "on " note-name)
      (= cmd note-off) (str "off  " note-name)
      :else            "nothing")))

(defn midi-notestring-on-track [track]
  (->> (midi-events-on-track track)
       (filter #(instance? ShortMessage (:msg %)))
       (map #(format "%s: %s"
                     (:tick %)
                     (note->str (:msg %))))))

(defn nth-track [midi-seq n]
  (nth (seq (.getTracks midi-seq)) n))

(defn load-midi-seq [filename]
  (MidiSystem/getSequence (io/as-file filename)))



(def midi-seq (atom nil))

(defn load-test-midi-seq
  []
  (reset! midi-seq
          (load-midi-seq "midi/heartbreaker.mid")))

(defn test-midi-notes []
  (midi-notestring-on-track (nth-track midi-seq 1)))

(defn extract-seq-with-tracks
  [old-seq track-nums]
  (let [div-type   (.getDivisionType old-seq)
        resolution (.getResolution old-seq)
        new-seq    (Sequence. div-type resolution)]
    (doseq [track-num track-nums]
      (let [old-track   (nth-track old-seq track-num)
            new-track   (.createTrack new-seq)
            events      (midi-events-on-track old-track)
            event-count (count events)]
        (loop [i      event-count
               events events]
          (when-not (zero? i)
            (let [event (:event (first events))]
              (.add new-track event)
              (recur (dec i) (rest events)))))))
    new-seq))

(defn write-seq-to-file
  [midi-seq file]
  (MidiSystem/write midi-seq
                    (first (seq (MidiSystem/getMidiFileTypes midi-seq)))
                    file))

(defn bytes->int
  "
  Converts signed bytes seq to int seq
  JVM Signed bytes will be converted to virtualized unsigned bytes
  "
  [bytes-int len]
  (reduce +
          (map #(bit-shift-left (if (neg? %1)
                                  (+ 256 %1)
                                  %1)
                                (* 8 %2))
               bytes-int
               (reverse (range len)))))

; 441176
; 5815046

(defn get-tempo
  [msg]
  (bytes->int (.getData msg) 3))

