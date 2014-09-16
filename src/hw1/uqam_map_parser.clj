(ns hw1.uqam_map_parser
  (:require [clojure.java.io :as io]
            clojure.string)
  (:gen-class))


(defn parse-part-1
  [line-to-parse node-map]
  (let [x (clojure.string/split (clojure.string/replace line-to-parse #"[(),]" "") #" ")]
    (assoc node-map (keyword (x 0))
           {:geo [(Float/parseFloat (x 1)) (Float/parseFloat (x 2))]})))

(defn parse-part-2
  [line-to-parse node-map]
  (let [x (clojure.string/split (clojure.string/replace line-to-parse #"[;:]" "") #"\s+")]
    (loop [ret-map node-map elements (rest x)]
      (if (= (count elements) 1)
        ret-map
        (recur (let [target-node (ret-map (keyword (first elements)))
                     kw-to-add (keyword (second elements))]
                 (assoc ret-map
                   (keyword (first elements))
                   (if (target-node :dest)
                    (assoc target-node :dest (cons kw-to-add (target-node :dest)))
                    (assoc target-node :dest [kw-to-add]))))
               (rest elements))))))

(defn parse-map
  "Parses file and outputs the node structure"
  [file-name]
  (with-open [reader (io/reader file-name)]

    (loop [rdr (line-seq reader) file-part 1 node-map {}]
      (if (empty? rdr)
        node-map
        (if (= (first rdr) "---")
          (recur (rest rdr) 2 node-map)
          (if (= 1 file-part)
           (recur (rest rdr) 1 (parse-part-1 (first rdr) node-map))
           (recur (rest rdr) 2 (parse-part-2 (first rdr) node-map)))))))

)
