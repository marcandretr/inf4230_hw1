(ns hw1.allstar
  (:require clojure.data.priority-map)
  (:gen-class))


; You can also call it A*

(defn- a-star-inner
  [world opened closed states goal heuristic-fn start-time]
  (if ((world :goal-satisfied?) (-> opened first first) goal)
    ; Moonwalk the parent map.e
    (let [final-time (System/currentTimeMillis)]
    (do
      ; Todo Bench
      (println (format "# Cost: %s" ((states (-> opened first first)) :g)))
      (println (format "# Generated states: %s" (count states)))
      (println (format "# Visited states (except Alabama): %s" (count closed)))
      (println (format "# A* time: %s ms" (- final-time start-time)))
      ; Reverse the path
      (loop [current-parent (states (-> opened first first)) final-path [(-> opened first first)]]
        (if current-parent
          (recur (states (current-parent :parent)) (cons (current-parent :parent) final-path))
          (rest (rest final-path))
          ))
      ))

    ; If goal is not the least expensive option
    (let [[new-opened new-closed new-states] ((world :gen-children-states)
                                              world
                                              states
                                              opened
                                              closed
                                              goal
                                              heuristic-fn)]
      (recur
        ; Return back the graph
        world
        new-opened
        new-closed
        new-states
        goal
        heuristic-fn
        start-time))))


(defn a-star
  [graph start-node end-node heuristic-fn]
  (a-star-inner
    graph
    (clojure.data.priority-map/priority-map start-node 0)
    {}
    {start-node {:g 0}}
    end-node
    heuristic-fn
    (System/currentTimeMillis)))


