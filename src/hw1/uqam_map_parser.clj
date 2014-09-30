(ns hw1.uqam_map_parser
  (:require [clojure.java.io :as io]
            [swiss.arrows :refer :all]
            clojure.string)
  (:gen-class))

(defn cost-of-move
  [world from to & _]
  (let [world-map (world :map)
        from (world-map from)
        to (world-map to)
        [from-lat from-lng] (from :geo)
        [to-lat to-lng] (to :geo)
        rad-deg-ratio (/ (Math/PI) 180)
        twice-earth-radius 12742000
        lng1 (* from-lng rad-deg-ratio)
        lng2 (* to-lng rad-deg-ratio)
        lat1 (* from-lat rad-deg-ratio)
        lat2 (* to-lat rad-deg-ratio)
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
                    {:geo [(Double/parseDouble (x 1)) (Double/parseDouble (x 2))]})))

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

(defn print-solution [states-seq]
  (if (= (second states-seq) nil)
    (println)
    (do
      (print (name (second states-seq)) "")
      (recur (rest states-seq)))))

(defn possible-next-states
  [world current-state _]
  (((world :map) current-state) :dest))

(defn parse-map
  "Parses file and outputs the node structure"
  [file-name]
  (with-open [reader (io/reader file-name)]

    (loop [rdr (line-seq reader)
           file-part 1
           node-map {}
           ways 0
           segments 0]
      (if (empty? rdr)
        (do
          (println (format "# Nodes: %s | Ways: %s | Segs: %s" (count node-map) ways segments))
          {
            :world {:map                  node-map
                    :cost-of-move         cost-of-move
                    :goal-satisfied?      (fn [state goal] (= state goal))
                    :printer              print-solution
                    :possible-next-states possible-next-states}
            }
          )
        (if (= (first rdr) "---")
          (recur (rest rdr) 2 node-map ways segments)
          (if (= 1 file-part)
            (recur (rest rdr) 1 (parse-part-1 (first rdr) node-map) segments ways)
            (let [[int-nodes-map acc-segs] (parse-part-2 (first rdr) node-map)]
              (recur (rest rdr) 2 int-nodes-map (inc ways) (+ segments acc-segs)))))))))


