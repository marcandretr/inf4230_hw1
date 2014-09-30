(ns hw1.allstar
  (:require clojure.data.priority-map)
  (:gen-class))

; You can also call it A*


(defn- generate-state-data
  "Update the opened, closed and states collections with the appropriates values according to new state"
  [world new-state-key parent-state-key opened closed states goal heuristic-fn]
  ; Calculate g and f of the current state
  (let [child-g (+ ((states parent-state-key) :g) ((world :cost-of-move) world parent-state-key new-state-key))
        child-f (+ child-g (heuristic-fn world new-state-key goal parent-state-key))]

    (cond (or
            ; if new state is in opened but have a better f
            (and
              (opened new-state-key)
              (> (opened new-state-key) child-f))

            ; or new state is not in closed nor opened
            (and
              (not (closed new-state-key))
              (not (opened new-state-key))))
          ; Return the collections with the new state
          [(assoc opened new-state-key child-f)
           closed
           (assoc states new-state-key {:g child-g :parent parent-state-key})]

          ; if new state is in closed but have a better f
          (and (closed new-state-key) (> (closed new-state-key) child-f))
          ; Return the collections with the new state
          [(assoc opened new-state-key child-f)
           (dissoc closed new-state-key)
           (assoc states new-state-key {:g child-g :parent parent-state-key})]

          :else
          ; Do not add the new state
          [opened closed states])))

(defn generate-new-states
  "Generate the new valid states accessibles from the current state."
  [world
   states
   opened
   closed
   goal
   heuristic-fn]

  ; Use the lowest cost state as the current state
  (let [current-state (-> opened first first)]

    ; We updates the lists for each possible new states if the meet the requirements
    (loop [possible-next-states ((world :possible-next-states) world current-state goal) ; Get all possible next states
           new-opened opened
           new-closed closed
           new-states states]

      (if (empty? possible-next-states)
        [(dissoc new-opened current-state)                  ; Return the new opened states without the current state
         (assoc new-closed current-state (opened current-state)) ; Return the closed states with the current state
         new-states]                                        ; Return the new data for the states

                                                            ; Update the opened, closed and states collection with the valid new states
        (let [current-child (first possible-next-states)
              [ret-opened ret-closed ret-states] (generate-state-data world current-child current-state new-opened new-closed new-states goal heuristic-fn)]

          (recur
            (rest possible-next-states)
            ret-opened
            ret-closed
            ret-states))))))

(defn- a-star-inner
  "The recursive part of the A* algorythm"
  [world opened closed states goal heuristic-fn start-time]
  ; if (goal is satisfied for the lowest cost state
  (if ((world :goal-satisfied?) (-> opened first first) goal)
    ; Moonwalk (ie. walk back) the parent map.e to construct the path
    (let [final-time (System/currentTimeMillis)]
      (do
        ;(println (format "# Cost: %s" ((states (-> opened first first)) :g)))
        ;(println (format "# Generated states: %s" (count states)))
        ;(println (format "# Visited states (except Alabama): %s" (count closed)))
        ;(println (format "# A* time: %s ms" (- final-time start-time)))

        ; Add each parents states from the initial state to the final path
        (loop [current-parent (states (-> opened first first)) final-path [(-> opened first first)]]
          (if current-parent
            (recur (states (current-parent :parent)) (cons (current-parent :parent) final-path))
            (rest final-path)                               ; Return the final path
            ))
        ))

    ; If goal is not the least expensive option
    (let [[new-opened new-closed new-states] (generate-new-states
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
  "A* algorythm used to find the shortest path in a graph."
  [graph initial-state goal heuristic-fn]
  (a-star-inner
    graph
    (clojure.data.priority-map/priority-map initial-state 0) ; We put the initial state in the "opened" map
    {}                                                      ; "closed" map
    {initial-state {:g 0}}                                  ; Keep the informations of the states ie. the cost of "g" and the "parent" if applicable
    goal
    heuristic-fn
    (System/currentTimeMillis)))


