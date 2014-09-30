(ns hw1.sokoban-parser
  (:require [clojure.java.io :as io]
            [clojure.set])
  (:gen-class))

; The block size is 64 because we can stock 64 blocks in binary representation in a Long (ie. bit ON for a block, OFF for nothing)
(defmacro BLOCK-SIZE [] 64)



; UNUSED START
(def deadlock-patterns-unidirectionnal
  [
    [[:wal :wal :emp]
     [:wal :emp :wal]
     [:emp :bal :wal]]

    [[:wal :wal :emp]
     [:wal :emp :wal]
     [:emp :bal :bal]]

    [[:wal :wal :emp]
     [:wal :emp :bal]
     [:emp :bal :wal]]

    [[:wal :wal :emp]
     [:wal :emp :bal]
     [:emp :bal :bal]]

    [[:wal :bal :emp]
     [:wal :emp :bal]
     [:emp :bal :bal]]

    [[:bal :wal :emp]
     [:wal :emp :bal]
     [:emp :bal :bal]]

    [[:bal :bal :emp]
     [:wal :emp :bal]
     [:emp :bal :bal]]

    [[:wal :bal :emp]
     [:bal :emp :bal]
     [:emp :bal :bal]]

    [[:bal :bal :emp]
     [:bal :emp :bal]
     [:emp :bal :bal]]

    [[:emp :wal :emp]
     [:wal :emp :wal]
     [:wal :bal :wal]]

    [[:emp :wal :emp]
     [:wal :emp :wal]
     [:wal :bal :bal]]

    [[:emp :wal :emp]
     [:wal :emp :bal]
     [:wal :bal :wal]]

    [[:emp :wal :emp]
     [:wal :emp :bal]
     [:wal :bal :bal]]

    [[:emp :wal :emp]
     [:wal :emp :wal]
     [:bal :bal :bal]]

    [[:emp :wal :emp]
     [:wal :emp :bal]
     [:bal :bal :bal]]

    [[:emp :wal :emp]
     [:bal :emp :bal]
     [:bal :bal :bal]]

    [[:emp :bal :wal]
     [:wal :bal :emp]
     [:emp :emp :emp]]])

(defn rotmat
  [[[a1 a2 a3] [b1 b2 b3] [c1 c2 c3]]]

  [[c1 b1 a1]
   [c2 b2 a2]
   [c3 b3 a3]])

(def all-deadlock-patterns

  (reduce concat
          (map
            (fn [shape]
              (let [r0 shape
                    r1 (rotmat r0)
                    r2 (rotmat r1)
                    r3 (rotmat r2)]
                [r0 r1 r2 r3])
              ) deadlock-patterns-unidirectionnal)))
; UNUSED END


; The functions representing the possible movements
(def move-fns
  [(fn [[x y]] [(inc x) y])
   (fn [[x y]] [(dec x) y])
   (fn [[x y]] [x (inc y)])
   (fn [[x y]] [x (dec y)])])


; Snippet from http://stackoverflow.com/a/3266877/2332936
; Modified to return the positions of the matches
; Return the positions of RegEx matches in a string
(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res []]
    (if (.find m)
      (recur m (conj res (.start m)))
      res)))
; End of snippet


(defn goal-satisfied?
  "Return true if the goal is satisfied."
  [[_ blocks] goal]
  (= blocks goal))

(defn cost-of-move
  "Return the cost to execute one movement. In sokoban, the cost is always 1."
  [& _]
  1)

(defn heuristic-1
  "Return 1 as the heuristique which transform our A* in a Djikstra algorythm"
  [world state goal]
  1)

; UNUSED
(defn- deadlocks?
  "Returns true if there is a deadlock otherwise false"
  [world state goal])

(defn- manathan-distance
  "Returns the manathan distance between 2 cells"
  [[x1 y1]
   [x2 y2]]

  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn- m-distance-to-closest-in-coord-set
  "Return the manathan distance of the positions and the closest coordinate of a collection"
  [position
   coord-set]
  (if (empty? coord-set)
    0 ; return 0 if there's nothing in the set
    (let [distance-map (map #(manathan-distance %1 position) coord-set)]
      (apply min distance-map)))) ; return the minimal distance after calculating distances between position and each coords

(defn- m-distance-closest-pair-eliminating
  ""
  [blocks goals]
  (loop [blocks blocks
         goals (transient (vec goals))
         distance 0]
    (if (empty? blocks)
      distance

      (let [[cost block-to-remove]
            (reduce (fn [[min-cost _ :as winning-block] current-block]
                      (let [current-cost (manathan-distance (nth goals 0) current-block)]
                        (if (> min-cost current-cost)
                          [current-cost current-block]
                          winning-block
                          ))) [Integer/MAX_VALUE nil] blocks)]

        (recur
          (disj blocks block-to-remove)
          (pop! goals)
          (+ distance cost))))))

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

(def pos-slices {
                  [-1 0] [[[-1 -1] [0 -1]
                           [-1 0] [0 0]]
                          [[-1 0] [0 0]
                           [-1 1] [0 1]]]
                  [1 0]  [[[0 -1] [1 -1]
                           [0 0] [1 0]]
                          [[0 0] [1 0]
                           [0 1] [1 1]]]
                  [0 -1] [[[-1 -1] [0 -1]
                           [-1 0] [0 0]]
                          [[0 -1] [1 -1]
                           [0 0] [1 0]]]
                  [0 1]  [[[-1 0] [0 0]
                           [-1 1] [0 1]]
                          [[0 0] [1 0]
                           [0 1] [1 1]]]
                  })

(defn dynamic-deadlock-detection
  [world
   [[dx dy :as dude-pos] boxes :as state]
   goals
   parent]


  (let [common-boxes (clojure.set/intersection boxes (second parent))]
    (if (= (second state) common-boxes)
      0
      (let [[cx cy :as child-box] (first (clojure.set/difference boxes common-boxes))
            trans-vec [(- cx dx) (- cy dy)]
            slices-rel (pos-slices trans-vec)
            content-of-slices
            (map
              (fn [slice]
                (map
                  (fn [[sx sy]] (or (contains? goals [(+ sx cx) (+ sy cy)]) (position-is-occupied-by world state [(+ sx cx) (+ sy cy)])))
                  slice))
              slices-rel)
            element-counter (fn [list-to-process] (reduce (fn [[blocker goals] content]
                                                              (case content
                                                                :block [(inc blocker) goals]
                                                                :wall [(inc blocker) goals]
                                                                true [blocker (inc goals)]
                                                                [blocker goals])) [0 0] list-to-process))

            [blocker-cnt goals-cnt] (element-counter (first content-of-slices))
            [blocker-cnt2 goals-cnt2] (element-counter (second content-of-slices))]

        (cond
          (or
           (= blocker-cnt 4)
           (= blocker-cnt2 4)) (Double/POSITIVE_INFINITY)
           :else 0)








        ))))

(defn heuristic [world current-state-key goals parent-state-key]
  ; Deadlocks
  (let [satisfied-goals (clojure.set/intersection (second current-state-key) goals)
        opened-boxes (clojure.set/difference (second current-state-key) satisfied-goals)
        opened-goals (clojure.set/difference goals satisfied-goals)
        res (+

              (m-distance-closest-pair-eliminating opened-boxes opened-goals)
              (max 0 (dec (m-distance-to-closest-in-coord-set (first current-state-key) opened-boxes)))
              (dynamic-deadlock-detection world current-state-key goals parent-state-key)
              )]

    (assert (>= res 0) (str "h() cannot be smaller than 0. h()=" res))
    res

    ))


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
  (if (not= (second states-seq) nil)
    (do
      (print (find-cardinal-point (-> states-seq first first) (-> states-seq second first)) "")
      (recur (rest states-seq)))
    (println)))

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
      [char-index line-num])))

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

(defn cell-is-dead?
  "Returns true if block is in a dead spot"
  [world position goal]

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

(defn is-valid-move
  [world
   [dude-position :as state]
   goal
   move-fn]

  (let [new-dude-position (move-fn dude-position)]

    (case (position-is-occupied-by world state new-dude-position)
      :wall false
      :empty true
      :block (let [box-pos (move-fn new-dude-position)]
               (and (= :empty (position-is-occupied-by world state box-pos))
                    (not (cell-is-dead? world box-pos goal))))

      ;(= :empty (position-is-occupied-by world state (move-fn new-dude-position)))
      )))

(defn generate-new-positions
  [world
   state
   goal]

  (for [move-fn move-fns
        :when (is-valid-move world state goal move-fn)]
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

(defn possible-next-states
  [world current-state goal]
  (map
    (partial generate-new-state-from-transform current-state)
    (generate-new-positions world current-state goal)))

(defn static-deadspot-analysis
  [map-longvec goals]

  (loop [opened (into (lazy-seq) goals)
         closed (lazy-seq)
         locked []]
    (if (empty? opened)
      locked
      (let [current-cell (first opened)]
        (if (or
              (contains? goals current-cell)
              )
          (recur (rest opened) (cons closed current-cell) locked)
          (recur (rest opened) (cons closed current-cell) (conj locked current-cell)))))))

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
        {:world       {:map                  map-longvec
                       :cost-of-move         cost-of-move
                       :goal-satisfied?      goal-satisfied?
                       :printer              print-solution
                       :possible-next-states possible-next-states}
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
