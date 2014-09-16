(ns hw1.allstar
  (:require clojure.data.priority-map))
; You can also call it A*


(defn- a-star-inner
  [graph opened closed goal fheuristic]
  (if (= (peek opened) goal)
    :found-path

    (recur
      graph
      (loop [futurly-opened (pop opened)
             keys-to-open (graph (peek opened))]
        (recur
          (assoc futurly-opened
            (peek keys-to-open)
            ((graph :move-cost) (pop opened) (peek keys-to-open)))
          (pop keys-to-open)))
      (cons (peek opened) closed)
      goal
      fheuristic)))


(defn a-star
  [graph start-node end-node fheuristic]
  (a-star-inner
    graph
    (clojure.data.priority-map/priority-map start-node 0)
    {}
    end-node
    fheuristic))


