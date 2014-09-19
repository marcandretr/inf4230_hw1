(ns hw1.allstar
  (:require clojure.data.priority-map)
  (:gen-class))

; You can also call it A*

(defn- a-star-inner
  [graph opened closed parents goal fheuristic visited-states]
  ; Parents should be stored in the node as a linked list
  ; When you drop a node, his parent chain is dropped automatically instead of keeping a map of parents
  (if (= ((peek opened) 0) goal)
    ; Walk back the parent map.
    (do
      ; Todo Bench
      (println (format "# Generated states: %s" (+ (count parents))))
      (println (format "# Visited states: %s" visited-states))
      ; Reverse the path
      (loop [current-parent (parents goal) final-path [goal]]
        (if current-parent
          (recur (parents (current-parent 0)) (cons (current-parent 0) final-path))
          (rest final-path)
          ))
      )

    ; If goal is not the least expensive option
    (let [[new-opened new-parents]
          (reduce
            ; The reduce function
            (fn [[op-map par-map] dest-kw-hrt]
              (if (and
                    (not (closed (dest-kw-hrt 0)))
                    (or
                      ; The open pmap does not contain
                      (not (contains? op-map (dest-kw-hrt 0)))
                      ;(> (op-map (dest-kw-hrt 0)) (dest-kw-hrt 1))
                       ))
                ; Return the new maps
                [(assoc op-map (dest-kw-hrt 0) (dest-kw-hrt 1))
                 (assoc par-map (dest-kw-hrt 0) (peek opened))]
                ; Return the same unmodified maps
                [op-map par-map]))
            ; The reduce initial value (so it's more like a fold)
            [(pop opened) parents]
            ; The list we are passing to the reduce
            (for [dest
                  (
                    (graph
                      ((peek opened) 0)
                      ) :dest)]
              [dest
               (+
                 (fheuristic (graph ((peek opened) 0)) (graph dest))
                 ((graph :move-cost)
                  (graph dest)
                  (graph goal)
                 )
                 )]

              ))]
      (do
        ;(println "# Visited" ((peek opened) 0))
      (recur
        ; Return back the graph
        graph
        new-opened
        (assoc closed ((peek opened) 0) ((peek opened) 0)  )
        new-parents
        goal
        fheuristic
        (inc visited-states))))))


(defn a-star
  [graph start-node end-node fheuristic]
  (a-star-inner
    graph
    (clojure.data.priority-map/priority-map start-node 0)
    {}
    {}
    end-node
    fheuristic
    0 ; Visited states
    ))


