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
    (((sokomap :world) :printer) (hw1.allstar/a-star (sokomap :world) (sokomap :first-state) (sokomap :goal) hw1.sokoban-parser/heuristic))))