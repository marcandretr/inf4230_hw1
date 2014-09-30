(ns hw1.core
  (:require hw1.uqam_map_parser
            hw1.osm_map_parser
            hw1.sokoban-parser
            hw1.allstar)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [map-path]

  (let [sokomap (hw1.sokoban-parser/parse-map map-path)]
  (((sokomap :world) :printer) (hw1.allstar/a-star (sokomap :world) (sokomap :first-state) (sokomap :goal) hw1.sokoban-parser/heuristic)))

;(defn uqam-star
;  [map-path]
;  (let [umap (hw1.uqam_map_parser/parse-map map-path)]
;  (((umap :world) :printer) (hw1.allstar/a-star (umap :world) :n1 :n8 hw1.uqam_map_parser/cost-of-move)))

  )