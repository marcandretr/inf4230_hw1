(ns hw1.sokoban-parser
  (:require
            [clojure.java.io :as io])
  (:gen-class))

(defn sobomap-to-vecs
  "Parses sokoban file and outputs a vector of vectors"
  [file-name]
  (with-open [reader (io/reader file-name)]
    (loop [rdr (line-seq reader) matmap []]
      (if (empty? rdr)
        matmap
        (recur
          (rest rdr)
          (conj
            matmap
            (reduce
            #(conj
              %1
              (cond
                (= %2 \space) :spce
                (= %2 \#) :wall
                (= %2 \.) :goal
                (= %2 \$) :blck
                (= %2 \@) :dude
                ; Maybe throw something
                :else "Oops... This was unexpected.")
              ) [] (seq (first rdr)))))))

    ))

(defn parse-map-beta
  [file-name]
  (loop [sobograph {}
         lines (map-indexed vector (sobomap-to-vecs file-name))] ; Line iteration
    (if (empty? lines)
      sobograph
      (recur
      (loop [sobograph-inner sobograph
             line (map-indexed vector ((first lines) 1))]; In line iteration
        (if (empty? line)
          sobograph-inner
          (recur
            (cond
              (= (second (first line)) :spce) ; How cute: a new node!
                (assoc sobograph-inner ; Todo: Check adjacent matrix cells and build dests.
                    (keyword (format "%d-%d" ((first lines) 0) (first (first line)))) 1)
              (= (second (first line)) :goal) sobograph-inner
              (= (second (first line)) :blck) sobograph-inner
              (= (second (first line)) :dude) sobograph-inner
              (= (second (first line)) :wall) sobograph-inner
              ; Maybe throw something
              :else "Oops... This was unexpected.")
            (rest line)))

        ) (rest lines))
      )

        ))

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