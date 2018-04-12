(ns nutrimat.core
  (:require [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream]))

(defn file->bytes [file]
  (with-open [xin (io/input-stream file)
              xout (ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

(defn -main
  []
  (nth (file->bytes "resources/hhgg.z3") 0x00))
