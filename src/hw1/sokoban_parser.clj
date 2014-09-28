(ns hw1.sokoban-parser
  (:require
            [clojure.java.io :as io])
  (:gen-class))

(defmacro BLOCK-SIZE [] 64)

(defn position-has-wall
  "Returns 1 if there is a block at the requested position
  Otherwise returns 0"
  [world
   [x y]]
  (let [linevec (world y)
        block (quot x (BLOCK-SIZE))
        idx-in-block (- x (* (BLOCK-SIZE) block))]
    (bit-and (bit-shift-right (linevec block) (- (dec (BLOCK-SIZE)) idx-in-block)) 1)))

(defn position-is-occupied
  [world
   [_ block-set]
   position]
    (or
      (block-set position)
      (position-has-wall world position)))
l
(defn generate-long-from-chunk
  [chunk]
    (reduce
      (fn
        [number ch]
        (bit-or (bit-shift-left number 1) (if (= \# ch) 2r1 0)))
      (long 0) chunk))

(defn generate-longvec-from-line
  [line]
  (let [chunk-size 64]
  (loop [line-chunks (partition chunk-size chunk-size (vec (repeat (dec chunk-size) \space)) line)
         longvec []]
    (if (empty? line-chunks)
      longvec
      (recur
        (rest line-chunks)
        (conj longvec (generate-long-from-chunk (first line-chunks)))))

    )
  ))

(defn parse-map
  [file-name]
  (with-open [reader (io/reader file-name)]
    (loop [rdr (line-seq reader)
           started-to-read false
           map-longvec []
           dude-pos []
           blocks-pos []
           ]
      (if (empty? rdr)
        map-longvec
        (recur
          (rest rdr)
          true
          (conj map-longvec (generate-longvec-from-line (first rdr)))
          dude-pos
          blocks-pos
          )

        ))

    )

  )