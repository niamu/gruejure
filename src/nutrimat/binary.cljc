(ns nutrimat.binary
  #?(:clj (:require [clojure.java.io :as io]))
  #?(:clj (:import [java.io ByteArrayOutputStream])))

;; Header addresses
(def header-main-address 0x06)
(def header-dictionary-table 0x08)
(def header-object-table 0x0a)
(def header-global-variable-table 0xc)
(def header-abbreviations-table 0x18)
(def header-routine-offset 0x28)
(def header-string-offset 0x2a)
(def header-alphabet-address 0x34)

#?(:clj
   (defn file->bytes [file]
     (with-open [xin (io/input-stream file)
                 xout (ByteArrayOutputStream.)]
       (io/copy xin xout)
       (.toByteArray xout))))

(defn byte-at
  [data address]
  (nth data address))

(defn set-byte-at [data address value]
  (aset-byte data address value))

(defn word-at
  [data address]
  (+ (bit-shift-left (nth data address) 8)
     (nth data (inc address))))

(defn set-word-at [data address value]
  (prn value)
  (let [high (bit-shift-right (bit-and 0xFF00 value) 8)
        low  (bit-and 0x00FF value)]
    (doto data
      (aset-byte address high)
      (aset-byte (inc address) low))))

(defn bits-set?
  [bits value]
  (= (bit-and bits value) bits))

(defn signed-word
  [word]
  (if (bit-test word 15)
    (- word 0x10000)
    word))
