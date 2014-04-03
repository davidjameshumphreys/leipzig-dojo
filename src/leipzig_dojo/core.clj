
(ns leipzig-dojo.core
(:use
    leipzig.melody
    leipzig.scale
    leipzig.canon
    leipzig.live
    leipzig.chord)

  (:require [overtone.live :as overtone]
            [overtone.synth.stringed :as strings])
  )


(strings/gen-stringed-synth ektara 1 true)

(defn pick [distort amp {midi :pitch, start :time, length :duration}]
    (let [synth-id (overtone/at start
                     (ektara midi :distort distort :amp amp :gate 1))]
      (overtone/at (+ start length) (overtone/ctl synth-id :gate 0))))

(defmethod play-note :leader [note]
  (pick 0.7 1.0 note))
(defmethod play-note :follower [note]
  (pick 0.3 1.0 note))
(defmethod play-note :bass [note]
  (pick 0.9 0.2 (update-in note [:pitch] #(- % 12))))

(def melody "A simple melody built from durations and pitches."
               ; Row, row, row your boat,
  (->> (phrase [3/3 3/3 2/3 1/3 3/3]
               [  0   0   0   1   2])
    (then
               ; Gently down the stream,
       (phrase [2/3 1/3 2/3 1/3 6/3]
               [  2   1   2   3   4]))
    (then
               ; Merrily, merrily, merrily, merrily,
       (phrase (repeat 12 1/3)
               (mapcat (partial repeat 3) [7 4 2 0])))
    (then
               ; Life is but a dream!
       (phrase [2/3 1/3 2/3 1/3 6/3]
               [  4   3   2   1   0]))
    (where :part (is :leader))))

(def bass "A bass part to accompany the melody."
  (->> (phrase [1  1 2]
               [0 -3 0])
     (where :part (is :bass))
     (times 4)))

(defn row-row
  "Play the tune 'Row, row, row your boat' as a round."
  [speed key]
  (->> melody
    (with bass)
    ;(times 2)
    (canon (comp (simple 4)
                 (partial where :part (is :follower))))
    (where :time speed)
    (where :duration speed)
    (where :pitch key)
    play))


(comment (defn chord [degree duration]
           (->> (-> triad (root degree)) vals (cluster duration) )))

(defn iws "Help"
  [speed key]
  (->> (phrase [4/4 4/4 4/4 4/4 4/4]
               [13   15     11   0.5  8])
       (times 1)
       (where :part (is :leader))
       (where :time speed)
       (where :duration speed)
       (where :pitch key)
       play))


(defn based-on-c [note]
  (when note
   (- (note) (C))))

(def close-encounter (phrase (repeat 4/4)
                             (map based-on-c [G A F (comp low F) C nil G A F (comp low F) C nil])
                             ))
(defn new-song
  []
  (let [sp (bpm 80)]
   (->> close-encounter
        (times 1)
        (where :part (is :leader))
        (where :time sp)
        (where :duration sp)
        (where :pitch C)
        play)))
