(ns hw1.allstar
  (:require clojure.data.priority-map)
  (:gen-class))

; You can also call it A*


(defn- generate-state-data
  [world child-key parent-key opened closed states goal heuristic-fn]
  (let [child-g (+ ((states parent-key) :g) ((world :cost-of-move) world parent-key child-key))
        child-f (+ child-g (heuristic-fn world child-key goal))]
    (cond (or
            (and
              (opened child-key)
              (> (opened child-key) child-f))

            (and
              (not (closed child-key))
              (not (opened child-key))))
          (do
            ;(prn "OP" "p" parent-key "c" child-key "| g" child-g "h" (heuristic-fn world child-key goal) "f" child-f )
            [(assoc opened child-key child-f)
             closed
             (assoc states child-key {:g child-g :parent parent-key})]
            )


          (and (closed child-key) (> (closed child-key) child-f))
          (do
            ;(prn "CL" "p" parent-key "c" child-key "| g" child-g "h" (heuristic-fn world child-key goal) "f" child-f )
          [(assoc opened child-key child-f)
           (dissoc closed child-key)
           (assoc states child-key {:g child-g :parent parent-key})])

          :else
          (do
            ;(prn "NA" "p" parent-key "c" child-key "| g" child-g "h" (heuristic-fn world child-key goal) "f" child-f )
          [opened closed states]))))

(defn generate-new-states
  [world
   states
   opened
   closed
   goal
   heuristic-fn]

  (let [current-state (-> opened first first)
        ;_ (println (-> opened first second))
        ]

    (loop [possible-next-states ((world :possible-next-states) world current-state goal)
           new-opened opened
           new-closed closed
           new-states states]

      (if (empty? possible-next-states)
        [(dissoc new-opened current-state)                  ; Return the new opened states with the processed one removed.
         (assoc new-closed current-state (opened current-state)) ; Return the new closed states.
         new-states]

        (let [current-child (first possible-next-states)
              [ret-opened ret-closed ret-states] (generate-state-data world current-child current-state new-opened new-closed new-states goal heuristic-fn)]

          (recur
            (rest possible-next-states)
            ret-opened
            ret-closed
            ret-states))))))

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
          (rest final-path)
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
  [graph start-node end-node heuristic-fn]
  (a-star-inner
    graph
    (clojure.data.priority-map/priority-map start-node 0)
    {}
    {start-node {:g 0}}
    end-node
    heuristic-fn
    (System/currentTimeMillis)))


