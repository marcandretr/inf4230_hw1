(ns hw1.osm_map_parser
  (:require [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [hw1.uqam_map_parser :as map]))

(defn parse-map
  "Parses file and outputs the node structure"
  [file-name]

  (let [check-one-way true]
    ; Read file
    (with-open [reader (io/reader file-name)]
      ; Loop with the xml tree, the map of nodes and all nodes key that are part of a way
      (loop [xml-doc (:content (xml/parse reader)) node-map {} nodes-in-way {}]
        ; If all the xml is consumed
        (if (= (count xml-doc) 0)
          ; Return all the nodes of the map that are part of a way
          (assoc (into {} (filter #(contains? nodes-in-way (key %)) node-map)) :move-cost map/cost-of-move)
          ; Else
          (let [element (first xml-doc)]
            ; If we got a node
            (if (= (:tag element) :node)
              (let [node (:attrs element)]
                ; Stock it in the map with his coordinates and loop again
                (recur (rest xml-doc)
                       (assoc node-map (keyword (str "n" (:id node))) {:geo [(read-string (:lat node)) (read-string (:lon node))]})
                       nodes-in-way))
              ; If we got a way
              (if (= (:tag element) :way)
                (let [[new-node-map new-nodes-in-way]
                      ; If it is a valid highway
                      (if
                          ; i.e. when the value of "highway" node is in the following map
                          (filter #(contains?
                                    ; Used a map because contains? work with keys when using map, but only work with indexes when using vectors
                                    {"residential" true "primary" true "primary_link" true "secondary" true "secondary_link" true "tertiary" true "tertiary_link" true "unclassified" true "cycleway" true "footway" true "service" true}
                                    (if (= (:k (:attrs %)) "highway")
                                      (:v (:attrs %))
                                      nil))
                                  (:content element))

                        (let [way-direction
                              (if check-one-way
                                (let [accepted-values {"yes" 1 "true" 1 "1" 1 "-1" -1 "reverse" -1}]
                                  (let [way-node (first (filter #(contains?
                                                           accepted-values
                                                           (if (= (:k (:attrs %)) "oneway")
                                                             (:v (:attrs %))
                                                             nil))
                                                         (:content element)))]
                                  (if way-node
                                      (accepted-values (:v (:attrs way-node)))
                                    0))
                                  )
                                0)]
                          (loop [nodes (filter #(= (:tag %) :nd) (:content element))
                                 ret-node-map node-map
                                 ret-nodes-in-way nodes-in-way]
                            (if (= (count nodes) 1)
                              [ret-node-map ret-nodes-in-way]
                              (let [nd1 (first nodes)
                                    nd2 (second nodes)]
                                (let [kw1 (keyword (str "n" (:ref (:attrs nd1))))
                                      kw2 (keyword (str "n" (:ref (:attrs nd2))))]
                                  (let [target-node1 (ret-node-map kw1)
                                        target-node2 (ret-node-map kw2)]
                                    (cond
                                      (= way-direction 1) (recur (rest nodes)
                                                                 (assoc ret-node-map
                                                                   kw1
                                                                   (if (:dest target-node1)
                                                                     (assoc target-node1 :dest (cons kw2 (target-node1 :dest)))
                                                                     (assoc target-node1 :dest [kw2])))
                                                                 (assoc (assoc ret-nodes-in-way
                                                                          kw2 true)
                                                                   kw1 true))
                                      (= way-direction -1) (recur (rest nodes)
                                                                  (assoc ret-node-map
                                                                    kw2
                                                                    (if (:dest target-node2)
                                                                      (assoc target-node2 :dest (cons kw1 (target-node2 :dest)))
                                                                      (assoc target-node2 :dest [kw1])))
                                                                  (assoc (assoc ret-nodes-in-way
                                                                           kw2 true)
                                                                    kw1 true))
                                      :else (recur (rest nodes)
                                                   (assoc
                                                       (assoc ret-node-map
                                                         kw1
                                                         (if (:dest target-node1)
                                                           (assoc target-node1 :dest (cons kw2 (target-node1 :dest)))
                                                           (assoc target-node1 :dest [kw2])))
                                                     kw2
                                                     (if (:dest target-node2)
                                                       (assoc target-node2 :dest (cons kw1 (target-node2 :dest)))
                                                       (assoc target-node2 :dest [kw1])))
                                                   (assoc (assoc ret-nodes-in-way
                                                            kw2 true)
                                                     kw1 true))
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ]
                  (recur (rest xml-doc)
                         new-node-map
                         new-nodes-in-way)
                  )
                ; ignore any other nodes and loop again
                (recur (rest xml-doc) node-map nodes-in-way)
                )
              )
            )
          )
        )
      )
    )
  )