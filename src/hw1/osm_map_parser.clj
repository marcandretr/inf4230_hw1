(ns hw1.osm_map_parser
  (:require [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]))

(defn parse-map
  "Parses file and outputs the node structure"
  [file-name]

  (with-open [reader (io/reader file-name)]
    (loop [xml-doc (:content (xml/parse reader)) node-map {} nodes-in-way {}]
      (if (= (count xml-doc) 0)
        (filter #(contains? nodes-in-way (key %)) node-map)
        (let [element (first xml-doc)]
          (if (= (:tag element) :node)
            (let [node (:attrs element)]
              (recur (rest xml-doc)
                     (assoc node-map (keyword (str "n" (:id node))) {:geo [(:lat node) (:lon node)]})
                     nodes-in-way))
            (if (= (:tag element) :way)
              (let [[new-node-map new-nodes-in-way]
                    (if
                          ; is it a highway?
                          ; si le v dans :k highway des :attrs d'1 :tag = kkch
                          (filter #(contains?
                                    {"residential" true "primary" true "primary_link" true "secondary" true "secondary_link" true "tertiary" true "tertiary_link" true "unclassified" true "cycleway" true "footway" true "service" true}
                                    (if (= (:k (:attrs %)) "highway")
                                      (:v (:attrs %))
                                      nil))
                                  (:content element))

                          ; Everything is two-way by default
                          (loop [nodes (:content element)
                                 ret-node-map node-map
                                 ret-nodes-in-way nodes-in-way]
                            (if (= (count nodes) 2)
                              [ret-node-map ret-nodes-in-way]
                              (let [nd1 (first nodes)
                                    nd2 (second nodes)]
                                (let [kw1 (keyword (str "n" (:ref (:attrs nd1))))
                                      kw2 (keyword (str "n" (:ref (:attrs nd2))))]
                                  (let [target-node1 (ret-node-map kw1)
                                        target-node2 (ret-node-map kw2)]
                                    (recur (rest nodes)
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
                                           ;
                                           (assoc (assoc ret-nodes-in-way
                                                    kw2 true)
                                             kw1 true)
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
              (recur (rest xml-doc) node-map nodes-in-way)
              )
            )
          )
        )
      )
    )
  )