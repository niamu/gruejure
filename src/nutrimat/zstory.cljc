(ns nutrimat.zstory)

(defonce story
  (atom {:filename ""
         :memory nil
         :frames []
         :breakpoints #{}}))

(defonce frame
  (atom {:pc nil
         :locals nil
         :stack nil
         :result-store nil}))

(defn create-frame
  [pc]
  {:pc pc
   :locals nil
   :stack nil
   :result-store nil})
