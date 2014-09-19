(ns hw1.sokoban-parser
  (:require [swiss.arrows :refer :all]
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

(defn parse-map
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