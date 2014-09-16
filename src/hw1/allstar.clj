(ns hw1.allstar
  (:require clojure.data.priority-map))
; You can also call it A*


(defn- a-star-inner
  [graph open closed goal fheuristic]








  )


(defn a-star
  [graph start-node end-node fheuristic]
  (a-star-inner
    graph
    (clojure.data.priority-map/priority-map start-node 0)
    {}
    end-node
    fheuristic))


