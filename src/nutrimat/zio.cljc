(ns nutrimat.zio)

(defn zprint [value]
  (print value))

(defn zprintln
  ([] (zprintln ""))
  ([value] (println value)))
