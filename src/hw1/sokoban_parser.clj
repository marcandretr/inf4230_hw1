(ns hw1.sokoban-parser
  (:require [clojure.java.io :as io])
  (:gen-class))

(defmacro BLOCK-SIZE [] 64)

; Snippet from http://stackoverflow.com/a/3266877/2332936
(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res []]
    (if (.find m)
      (recur m (conj res (.start m)))
      res)))

(defn position-has-wall
  [{world :map}
   [x y]]
  (let [linevec (world y)
        block (quot x (BLOCK-SIZE))
        idx-in-block (- x (* (BLOCK-SIZE) block))]
    (= 1 (bit-and
      (bit-shift-right
        (linevec block)

        (-
          (dec
            (BLOCK-SIZE))
          idx-in-block)) 1))))

(defn position-is-occupied-by
  [world
   [_ block-set]
   position]
  (cond
    (block-set position) :block
    (position-has-wall world position) :wall
    :else :empty))

(defn generate-long-from-chunk
  [chunk]
  (reduce
    (fn
      [number ch]
      (bit-or (bit-shift-left number 1) (if (= \# ch) 2r1 0)))
    (long 0) chunk))

(defn find-dude-pos
  [[line-num line] dude-pos]
  (let [char-index (.indexOf line "@")]
    (if (= char-index -1)
      dude-pos
      [char-index line-num]
      )))

(defn find-pos
  [line-num line blocks-pos symbol-regex]
  (reduce #(conj %1 (vec [%2 line-num])) blocks-pos (hw1.sokoban-parser/re-pos symbol-regex line)))

(defn find-blocks-pos
  [[line-num line] blocks-pos]
  (find-pos line-num line blocks-pos #"\$"))

(defn find-goals-pos
  [[line-num line] goals-pos]
  (find-pos line-num line goals-pos #"\."))

(defn generate-longvec-from-line
  [line]
  (let [chunk-size 64]
    (loop [line-chunks (partition chunk-size chunk-size (vec (repeat (dec chunk-size) \space)) line)
           longvec []]
      (if (empty? line-chunks)
        longvec
        (recur
          (rest line-chunks)
          (conj longvec (generate-long-from-chunk (first line-chunks))))))))

(defn is-valid-move
  [world
   [dude-position _ :as state]
   move-fn]

  (let [new-dude-position (move-fn dude-position)]

  (case (position-is-occupied-by world state new-dude-position)
    :wall false
    :empty true
    :block (= :empty (position-is-occupied-by world state (move-fn new-dude-position))))))

(defn generate-new-positions
  [world
   state]

  (for [move-fn [(fn [[x y]] [(inc x) y])
             (fn [[x y]] [(dec x) y])
             (fn [[x y]] [x (inc y)])
             (fn [[x y]] [x (dec y)])]
        :when (is-valid-move world state move-fn)]
    move-fn)

  ;(filter (fn [move-fn] (is-valid-move world state move-fn))
  ;        [(fn [[x y]] [(inc x) y])
  ;         (fn [[x y]] [(dec x) y])
  ;         (fn [[x y]] [x (inc y)])
  ;         (fn [[x y]] [x (dec y)])])

  )

(defn generate-new-states
  [world states opened closed goal] )

(defn parse-map
  [file-name]
  (with-open [reader (io/reader file-name)]
    (loop [rdr (map-indexed vector (line-seq reader))
           started-to-read false
           map-longvec []
           dude-pos []
           blocks-pos (sorted-set)
           goals-pos (sorted-set)]
      (if (empty? rdr)
        {:world {:map map-longvec :gen-children-states generate-new-states} :first-state [dude-pos blocks-pos] :goal goals-pos}
        (let [line (first rdr)]
          (recur
            (rest rdr)
            true
            (conj map-longvec (generate-longvec-from-line (second (first rdr))))
            (if (empty? dude-pos)
              (find-dude-pos line dude-pos)
              dude-pos)
            (find-blocks-pos line blocks-pos)
            (find-goals-pos line goals-pos)))))))