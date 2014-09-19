(ns hw1.core
  (:require hw1.uqam_map_parser
            hw1.osm_map_parser
            hw1.allstar)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (let [heurist (fn [from to] (hw1.uqam_map_parser/cost-of-move from to))
        start-time (System/currentTimeMillis)]

    (let [ans (hw1.allstar/a-star
                (hw1.uqam_map_parser/parse-map "resources/uqam-map-1.txt")
                :n1
                :n10
                heurist)]
      (println (format "# Elapsed time: %s ms" (- (System/currentTimeMillis) start-time)))
      ans
      ))

  ;(def omap (hw1.osm_map_parser/parse-map "resources/carte-osm-simple.osm"))

  ;(println (hw1.allstar/a-star omap :n2 :n3 heurist1))

  )