(ns hw1.allstar
  (:require clojure.data.priority-map)
  (:gen-class))

; You can also call it A*

(defn- a-star-inner
  [graph opened closed states goal fheuristic visited-states]
  (if (= ((peek opened) 0) goal)
    ; Walk back the parent map.
    (do
      ; Todo Bench
      (println (format "# Generated states: %s" (count states)))
      (println (format "# Visited states: %s" visited-states))
      ; Reverse the path
      (loop [current-parent (states goal) final-path [goal]]
        (if current-parent
          (recur (states (current-parent 0)) (cons (current-parent 0) final-path))
          (rest final-path)
          ))
      )

    ; If goal is not the least expensive option
    (let [[new-opened new-parents] ]
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
        (inc visited-states))))


    ))


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


