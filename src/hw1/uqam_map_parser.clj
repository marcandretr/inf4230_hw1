(ns hw1.uqam_map_parser
  (:require [clojure.java.io :as io]
            [swiss.arrows :refer :all]
            clojure.string)
  (:gen-class))

(defn cost-of-move
  [world from to]
  (let [from (world from)
        to (world to)
        [from-lat from-lng] (from :geo)
        [to-lat to-lng] (to :geo)
        rad-deg-ratio (/ (Math/PI) 180)
        twice-earth-radius 12742000
        lng1 (/ (* from-lng Math/PI) 180)
        lng2 (/ (* to-lng Math/PI) 180)
        lat1 (/ (* from-lat Math/PI) 180)
        lat2 (/ (* to-lat Math/PI) 180)
        s1 (Math/sin (/ (- lat2 lat1) 2))
        s2 (Math/sin (/ (- lng2 lng1) 2))]

    (* twice-earth-radius
       (Math/asin
         (Math/sqrt
           (+
             (Math/pow s1 2)
             (*
               (Math/cos lat1)
               (Math/cos lat2)
               (Math/pow s2 2))))))))


(defn parse-part-1
  [line-to-parse node-map]
  (let [x (clojure.string/split (clojure.string/replace line-to-parse #"[(),]" " ") #"\s+")]
    (assoc node-map (keyword (x 0))
                    {:geo [(Float/parseFloat (x 1)) (Float/parseFloat (x 2))]})))

(defn parse-part-2
  [line-to-parse node-map]
  (let [x (clojure.string/split (clojure.string/replace line-to-parse #"[;:]" " ") #"\s+")]
    (loop [ret-map node-map elements (rest x)]
      (if (= (count elements) 1)
        [ret-map (-> x rest count)]
        (recur (let [target-node (ret-map (keyword (first elements)))
                     kw-to-add (keyword (second elements))]
                 (assoc ret-map
                   (keyword (first elements))
                   (if (target-node :dest)
                     (assoc target-node :dest (cons kw-to-add (target-node :dest)))
                     (assoc target-node :dest [kw-to-add]))))
               (rest elements))))))

(defn- generate-one-state
  [child-key child-f child-g parent-key opened closed states]
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
        [opened closed states]))

(defn generate-new-states
  "Finds the new childrens and their cost"
  [world states opened closed goal]
  (let [parent-state-key ((first opened) 0)]
    (loop [possible-destinations ((world parent-state-key) :dest)
           new-opened opened
           new-closed closed
           new-states states]
      (if (empty? possible-destinations)
        [(dissoc new-opened parent-state-key)
         (assoc new-closed parent-state-key (new-opened parent-state-key))
         new-states]

        (let [current-child-g (+ ((states parent-state-key) :g)
                                 (cost-of-move world parent-state-key (first possible-destinations)))]
          (let [[ret-opened ret-closed ret-states]
                (generate-one-state (first possible-destinations)
                                    (+ current-child-g (cost-of-move world (first possible-destinations) goal))
                                    current-child-g
                                    parent-state-key
                                    new-opened
                                    new-closed
                                    new-states)]
            (recur
              (rest possible-destinations)
              ret-opened
              ret-closed
              ret-states)))))))


(defn parse-map
  "Parses file and outputs the node structure"
  [file-name]
  (with-open [reader (io/reader file-name)]

    (loop [rdr (line-seq reader)
           file-part 1
           node-map {}
           ways 0
           segments 0
           ]
      (if (empty? rdr)
        (do
          (println (format "# Nodes: %s | Ways: %s | Segs: %s" (count node-map) ways segments))
          (assoc node-map :gen-children-states generate-new-states))
        (if (= (first rdr) "---")
          (recur (rest rdr) 2 node-map ways segments)
          (if (= 1 file-part)
            (recur (rest rdr) 1 (parse-part-1 (first rdr) node-map) segments ways)
            (let [[int-nodes-map acc-segs] (parse-part-2 (first rdr) node-map)]
              (recur (rest rdr) 2 int-nodes-map (inc ways) (+ segments acc-segs))))))))

  )


