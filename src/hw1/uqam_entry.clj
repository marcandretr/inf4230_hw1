(ns hw1.uqam-entry
  (:require hw1.uqam_map_parser
            hw1.osm_map_parser
            hw1.allstar)
  (:gen-class))

(defn -main
  "Entrypoint for Beaudry Map Solver"
  [map-path kw1 kw2]

    (let [umap (hw1.uqam_map_parser/parse-map map-path)]
    (((umap :world) :printer) (hw1.allstar/a-star (umap :world) (keyword kw1) (keyword kw2) hw1.uqam_map_parser/cost-of-move))))