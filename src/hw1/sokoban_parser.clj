(ns hw1.sokoban-parser
  (:require [clojure.java.io :as io]
            [clojure.set])
  (:gen-class))

(defmacro BLOCK-SIZE [] 64)



(def deadlock-patterns-unidirectionnal
  [
    ; [[:any :any]
    ; [:any :any]]

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
  (let [r0 deadlock-patterns-unidirectionnal
        r1 (rotmat r0)
        r2 (rotmat r1)
        r3 (rotmat r2)]
    (reduce concat [r0 r1 r2 r3])))



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
  [world state goal])

(defn- manathan-distance
  "Returns the manathan distance between 2 cells"
  [[x1 y1]
   [x2 y2]]

  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn- m-distance-to-closest-in-coord-set
  [position
   coord-set]
  (if (empty? coord-set)
    0
    (let [distance-map (map #(manathan-distance %1 position) coord-set)]

      (apply min distance-map)))
  )


(defn- m-distance-to-closest-pair
  [blocks goals]

  (let [v (reduce #(+ %1 (m-distance-to-closest-in-coord-set %2 goals)) 0 blocks)]
    ;(println v)
    v

    ))

(defn- m-distance-closest-pair-eliminating
  [blocks goals]
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
  (let [satisfied-goals (clojure.set/intersection (second state) goals)
        opened-boxes (clojure.set/difference (second state) satisfied-goals)
        opened-goals (clojure.set/difference goals satisfied-goals)
        res (+

              ;(m-distance-closest-pair-eliminating opened-boxes opened-goals)
              ;(m-distance-to-closest-pair opened-boxes opened-goals)
              (max 0 (dec (m-distance-to-closest-in-coord-set (first state) opened-boxes)))
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
      (recur (rest states-seq))
      )
    (println)
    ))

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

  (for [move-fn [(fn [[x y]] [(inc x) y])
                 (fn [[x y]] [(dec x) y])
                 (fn [[x y]] [x (inc y)])
                 (fn [[x y]] [x (dec y)])]
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
