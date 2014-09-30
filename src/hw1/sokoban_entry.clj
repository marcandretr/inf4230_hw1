(ns hw1.sokoban-entry
  (:require hw1.sokoban-parser
            hw1.allstar)
  (:gen-class))

(defn -main
  "Entrypoint for Sokoban Solver"
  [map-path]
  (let [sokomap (hw1.sokoban-parser/parse-map map-path)]
    (((sokomap :world) :printer) (hw1.allstar/a-star (sokomap :world) (sokomap :first-state) (sokomap :goal) hw1.sokoban-parser/heuristic))))
