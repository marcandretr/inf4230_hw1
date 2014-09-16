(ns hw1.core
  (:require hw1.uqam_map_parser)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (hw1.uqam_map_parser/parse-map "resources/uqam-map-1.txt")))
