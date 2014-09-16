(ns hw1.allstar
  (:require clojure.data.priority-map))
; You can also call it A*


(defn- a-star-inner
  [graph opened closed parents goal fheuristic]
  (if (= (peek opened) goal)
    ; Todo: Walk back the parent map.
    :found-path

    (let [[new-opened new-parents]
          (reduce
            (fn [[op-map par-map] dest-kw-hrt]
              (if (or
                    ; The open pmap does not contain
                    (not (contains? op-map (dest-kw-hrt 0)))
                    (> (op-map (dest-kw-hrt 0)) (dest-kw-hrt 1)))
                ; Replace

                ; Return the new maps
                [(assoc op-map (dest-kw-hrt 0) (dest-kw-hrt 1))
                 (assoc par-map (dest-kw-hrt 0) (peek opened))]

                ; Return the same input
                [op-map par-map]))
            [(pop opened) parents]
            ((graph (peek opened)) :dest))]

      (recur
        ; Return back the graph
        graph
        new-opened
        (cons (peek opened) closed)
        new-parents
        goal
        fheuristic)


      )
    ))


(defn a-star
  [graph start-node end-node fheuristic]
  (a-star-inner
    graph
    (clojure.data.priority-map/priority-map start-node 0)
    {}
    {}
    end-node
    fheuristic))


