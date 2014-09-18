(ns hw1.core
  (:require hw1.uqam_map_parser
            hw1.osm_map_parser
            hw1.allstar)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def umap (hw1.uqam_map_parser/parse-map "resources/uqam-map-1.txt"))
  (println umap)

  (defn heurist1
    [from to]
    (hw1.uqam_map_parser/cost-of-move from to))

  (hw1.allstar/a-star umap :n1 :n8 heurist1)


  ;(hw1.osm_map_parser/parse-map "resources/carte-osm.osm")

  )