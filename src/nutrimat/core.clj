(ns nutrimat.core
  (:require [nutrimat.binary :as binary]
            [nutrimat.zins :as zins]
            [nutrimat.zmachine :as zmachine]
            [nutrimat.zmem :as zmem]
            [nutrimat.zops]
            [nutrimat.zstory :as zstory]))

(defn load-frame
  [pc]
  (swap! zstory/frame assoc
         :pc pc
         :locals nil
         :stack nil
         :result-store nil))

(defn load-story
  [filename]
  (swap! zstory/story assoc
         :filename filename
         :memory (binary/file->bytes filename)
         :frames [])
  (swap! zstory/story update-in
         [:frames]
         #(conj % (load-frame (binary/word-at (:memory @zstory/story)
                                              zmem/header-main-address)))))

(defn peek-instruction []
  (let [current-pc (zmachine/pc)
        ins (zins/read-instruction)]
    (zmachine/set-pc current-pc)
    ins))

(defn -main
  []
  (load-story "resources/zork.dat"))

#_(zmem/version)
@zstory/story
@zstory/frame

#_(zins/execute-instruction (zins/read-instruction))
