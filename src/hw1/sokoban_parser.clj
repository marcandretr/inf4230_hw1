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
; End of snippet

(defn goal-satisfied?
  [[_ blocks]
   goal]
  (= blocks goal)
  )

(defn cost-of-move [& _] 1)

(defn heuristic-1 [world state goal]
  1)

(defn- deadlocks?
  "Returns true if there is a deadlock; otherwise false"
  [world state goal]

  )

(defn- manathan-distance
  "Returns the manathan distance between 2 cells"
  [[x1 y1]
   [x2 y2]]

  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn- m-distance-to-closest-goal
  [position
   goals]
  (apply min (map #(manathan-distance %1 position) goals)))

(defn- m-distance-to-closest-pair
  [[_ blocks :as state] goals]

  (let [v (reduce #(+ %1 (m-distance-to-closest-goal %2 goals)) 0 blocks)]
    ;(println v)
    v

    ))

(defn- m-distance-closest-pair-eliminating
  [[_ blocks :as state] goals]
  (loop [blocks blocks
         goals goals
         distance 0]
    (if (empty? goals)
      distance


    (let [block-goals-combination-distance (for [b blocks
                                                 g goals
                                                 :let [bgd [(manathan-distance b g) b g]]]
                                             bgd)
          closest-pair (apply (partial min-key #(first %1)) block-goals-combination-distance)]
      (recur
        (disj blocks (nth closest-pair 1))
        (disj goals (nth closest-pair 2))
        (+ distance (first closest-pair)))))))

(defn heuristic [world state goals]
  ; Deadlocks

  (+

    ;(* 1 (m-distance-closest-pair-eliminating state goals))
    (m-distance-to-closest-pair state goals)
    )

  )


(defn- find-cardinal-point
  [[x1 y1]
   [x2 y2]]
  [(- x2 x1) (- y2 y1)]

  (cond
    (> y2 y1) "S"
    (< y2 y1) "N"
    (> x2 x1) "E"
    (< x2 x1) "W"))

(defn print-solution [states-seq]
  (when (not= (second states-seq) nil)
    (print (find-cardinal-point (-> states-seq first first) (-> states-seq second first)) "")
    (recur (rest states-seq))))

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
   [_ block-set :as state]
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
   [dude-position :as state]
   move-fn]

  (let [new-dude-position (move-fn dude-position)]

    (case (position-is-occupied-by world state new-dude-position)
      :wall false
      :empty true
      :block (= :empty (position-is-occupied-by world state (move-fn new-dude-position)))
      ;(= :empty (position-is-occupied-by world state (move-fn new-dude-position)))
      :fuck)))

(defn generate-new-positions
  [world
   state]

  (for [move-fn [(fn [[x y]] [(inc x) y])
                 (fn [[x y]] [(dec x) y])
                 (fn [[x y]] [x (inc y)])
                 (fn [[x y]] [x (dec y)])]
        :when (is-valid-move world state move-fn)]
    move-fn))

(defn generate-new-state-from-transform
  "Returns a new state assuming that the transform given opens on a valid state."
  [[dude-pos blocks]
   transform-fn]
  (let [new-dude-pos (transform-fn dude-pos)
        new-blocks (if (contains? blocks new-dude-pos)
                     (conj (disj blocks new-dude-pos) (transform-fn new-dude-pos))
                     blocks)]
    (conj [new-dude-pos] new-blocks)))

(defn- generate-state-data
  [world child-key parent-key opened closed states goal heuristic-fn]
  (let [child-g (+ ((states parent-key) :g) ((world :cost-of-move) world child-key parent-key))
        child-f (+ child-g (heuristic-fn world child-key goal))
        ;_ (println child-f)
        ]
    (cond (or
            (and
              (opened child-key)
              (> (opened child-key) child-f))
            (not (closed child-key))
            )
          [(assoc opened child-key child-f)
           closed
           (assoc states child-key {:g child-g :parent parent-key})]

          (and (closed child-key) (> (closed child-key) child-f))
          [(assoc opened child-key child-f)
           (dissoc closed child-key)
           (assoc states child-key {:g child-g :parent parent-key})]

          :else
          [opened closed states])))

(defn generate-new-states
  [world
   states
   opened
   closed
   goal
   heuristic-fn]

  (let [current-state (-> opened first first)]

    (loop [possible-next-states (map
                                  (partial generate-new-state-from-transform current-state)
                                  (generate-new-positions world current-state))
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
            ret-states
            )

          )

        )
      ))

  )

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
        {:world       {:map                 map-longvec
                       :cost-of-move        cost-of-move
                       :gen-children-states generate-new-states
                       :goal-satisfied?     goal-satisfied?
                       :printer             print-solution}
         :first-state [dude-pos blocks-pos]
         :goal        goals-pos}
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



; Static analysis functions
(defn cell-is-dead?
  "Returns true if block is in a dead spot"
  [world [position blocs :as state] goal]

  ; if in corner && not a goals
  (if (contains? goal position)
    false
    (let [bottom (= :wall (position-is-occupied-by world [[] #{}] ((fn [[x y]] [(inc x) y]) position)))
          top (= :wall (position-is-occupied-by world [[] #{}] ((fn [[x y]] [(dec x) y]) position)))
          right (= :wall (position-is-occupied-by world [[] #{}] ((fn [[x y]] [x (inc y)]) position)))
          left (= :wall (position-is-occupied-by world [[] #{}] ((fn [[x y]] [x (dec y)]) position)))]
      (and (or (and top left)
               (and top right)
               (and bottom left)
               (and bottom right))))))

(defn analysis-dead-cells
  [world goal]
  (loop [opened [(first goal)]
         closed []
         dead-cells []]
    (if (= (count opened) 0)
      dead-cells
      (let [current-pos (first opened)
            current-state [current-pos #{}]
            possible-next-states (map
                                   (partial generate-new-state-from-transform current-state)
                                   (generate-new-positions world current-state))
            is-dead? (cell-is-dead? world current-state goal)]
        (recur (apply (partial conj (rest opened))
                      (filter
                        #(not (or (contains? opened (first %)) (contains? closed (first %))))
                        possible-next-states))
               (conj closed current-pos)
               (if is-dead?
                 (conj dead-cells current-pos)
                 dead-cells))))))
; End of static analysis functions