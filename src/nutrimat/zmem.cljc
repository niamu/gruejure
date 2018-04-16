(ns nutrimat.zmem
  (:require [nutrimat.binary :as binary]
            [nutrimat.util :refer [defmethod-for-values]]
            [nutrimat.zstory :as zstory]))

;; Memory
(defn zbyte [byte-addr]
  (binary/byte-at (:memory @zstory/story) byte-addr))

(defn set-zbyte [byte-addr value]
  (swap! zstory/story update-in [:memory]
         #(binary/set-byte-at % byte-addr value)))

(defn zword [word-addr]
  (binary/word-at (:memory @zstory/story) word-addr))

(defn set-zword [word-addr value]
  (swap! zstory/story update-in [:memory]
         #(binary/set-word-at % word-addr value)))

(defn version
  ([] (zbyte 0))
  ([&rest] (zbyte 0)))

;; Header addresses
(def header-version 0x00)
(def header-main-address 0x06)
(def header-dictionary-table 0x08)
(def header-object-table 0x0a)
(def header-global-variable-table 0xc)
(def header-abbreviations-table 0x18)
(def header-routine-offset 0x28)
(def header-string-offset 0x2a)
(def header-alphabet-address 0x34)

;; Addressing
(defmulti routine-address version)
(defmethod-for-values routine-address '(1 2 3) [packed] (* 2 packed))
(defmethod-for-values routine-address '(4 5) [packed] (* 4 packed))
(defmethod-for-values routine-address '(6 7) [packed]
  (+ (* 4 packed) (* 8 (zword header-routine-offset))))
(defmethod-for-values routine-address '(8) [packed] (* 8 packed))

(defmulti string-address version)
(defmethod-for-values string-address '(1 2 3) [packed] (* 2 packed))
(defmethod-for-values string-address '(4 5) [packed] (* 4 packed))
(defmethod-for-values string-address '(6 7) [packed]
  (+ (* 4 packed) (* 8 (zword header-string-offset))))
(defmethod-for-values string-address '(8) [packed] (* 8 packed))
