(ns hw1.core
  (:require hw1.uqam_map_parser
            hw1.osm_map_parser
            hw1.sokoban-parser
            hw1.allstar)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [map-path]

  ;(let [skw (keyword start-kw)
  ;      ekw (keyword end-kw)
  ;      map (hw1.uqam_map_parser/parse-map "resources/carte-montreal.txt")
  ;      start-time (System/currentTimeMillis)]
  ;
  ;  (let [ans (hw1.allstar/a-star
  ;              map
  ;              skw
  ;              ekw)]
  ;    (println (format "# Elapsed time: %s ms" (- (System/currentTimeMillis) start-time)))
  ;    ans))




  ;(def omap (hw1.osm_map_parser/parse-map "resources/carte-osm-simple.osm"))

  ;(println (hw1.allstar/a-star omap :n2 :n3 heurist1))

  (def a (hw1.sokoban-parser/parse-map map-path))
  (hw1.allstar/a-star (a :world) (a :first-state) (a :goal) hw1.sokoban-parser/heuristic-1)

  )