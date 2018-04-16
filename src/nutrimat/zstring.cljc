(ns nutrimat.zstring
  (:refer-clojure :exclude [read-string])
  (:require [nutrimat.util :as util :refer [defmethod-for-values]]
            [nutrimat.zmem :as zmem]))

(def alphabet-a0-standard "      abcdefghijklmnopqrstuvwxyz")
(def alphabet-a1-standard "      ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def alphabet-a2-standard "       \n0123456789.,!?_#'\"/\\-:()")

(defmulti alphabet-a0 zmem/version)
(defmethod-for-values alphabet-a0 '(1 2 3 4) [] alphabet-a0-standard)
(defmethod-for-values alphabet-a0 '(5 6 7 8) []
  (let [custom-alphabet-address (zmem/zword zmem/header-alphabet-address)]
    (if (zero? custom-alphabet-address)
      alphabet-a0-standard
      (util/error "Don't support custom alphabets yet"))))

(defmulti alphabet-a1 zmem/version)
(defmethod-for-values alphabet-a1 '(1 2 3 4) [] alphabet-a1-standard)
(defmethod-for-values alphabet-a1 '(5 6 7 8) []
  (let [custom-alphabet-address (zmem/zword zmem/header-alphabet-address)]
    (if (zero? custom-alphabet-address)
      alphabet-a1-standard
      (util/error "Don't support custom alphabets yet"))))

(defmulti alphabet-a2 zmem/version)
(defmethod alphabet-a2 1 [] "      0123456789.,!?_#'\"/\\<-:()")
(defmethod-for-values alphabet-a2 '(2 3 4) [] alphabet-a2-standard)
(defmethod-for-values alphabet-a2 '(5 6 7 8) []
  (let [custom-alphabet-address (zmem/zword zmem/header-alphabet-address)]
    (if (zero? custom-alphabet-address)
      alphabet-a2-standard
      (util/error "Don't support custom alphabets yet"))))

(def alphabets { :a0 alphabet-a0 :a1 alphabet-a1 :a2 alphabet-a2})

(defonce current-alphabet (atom [:a0 nil]))
(defn set-alphabet [alpha shift]
  (dosync (reset! current-alphabet [alpha shift]))
  nil)

(defonce text-address (atom nil))
(defonce text-end (atom false))
(defonce read-zchars (atom nil))
(defonce num-zwords (atom 0))

(defn next-zchars []
  (let [bytes (zmem/zword @text-address)]
    (if (zero? @num-zwords)
      (dosync
       (reset! text-address nil)
       (reset! text-end true)
       (reset! read-zchars nil))
      (dosync
       (swap! text-address #(+ % 2))
       (reset! text-end (bit-test bytes 15))
       (reset! read-zchars
               [(bit-shift-right (bit-and bytes 0x7C00) 10)
                (bit-shift-right (bit-and bytes 0x3E0) 5)
                (bit-and bytes 0x1F)])
       (swap! num-zwords dec)))))

(defn next-zchar []
  (when (and (not @text-end) (nil? @read-zchars))
    (next-zchars))
  (let [zch (first @read-zchars)]
    (dosync (reset! read-zchars (rest @read-zchars)))
    zch))

(def shift-table-1 {:a0 :a1, :a1 :a2, :a2 :a0})
(def shift-table-2 {:a0 :a2, :a1 :a0, :a2 :a1})
(def shift-table {2 shift-table-1
                  3 shift-table-2
                  4 shift-table-1
                  5 shift-table-2})

(defn lookup-in-alphabet [index]
  (let [ch (.charAt ((alphabets (first @current-alphabet))) index)]
    (when (= (second @current-alphabet) :shift)
      (set-alphabet :a0 nil))
    ch))

(declare read-string)

(defn abbreviation [group index]
  ;; TODO: replace the string reading state with thread local vars instead of refs
  (let [alphabet @current-alphabet
        address @text-address
        end @text-end
        zchars @read-zchars
        abbrev-address (+ (zmem/zword zmem/header-abbreviations-table)
                          (* 2 (+ (* 32 (dec group)) index)))
        abbrev (read-string (* 2 (zmem/zword abbrev-address)))]
    (dosync (reset! current-alphabet alphabet)
            (reset! text-address address)
            (reset! text-end end)
            (reset! read-zchars zchars))
    abbrev))

(defn alphabet-shift-for [shift]
  ((shift-table shift) @current-alphabet))

(defn multi-zchar []
  (let [ch (char (bit-or (bit-shift-left (next-zchar) 5) (next-zchar)))]
    (when (= (second @current-alphabet) :shift)
      (set-alphabet :a0 nil))
    ch))

(defmulti process-zchar zmem/version)
(defmethod-for-values process-zchar '(1 2) [ch]
  (cond (<= 2 ch 3) (set-alphabet (alphabet-shift-for ch) :shift)
        (<= 4 ch 5) (set-alphabet (alphabet-shift-for ch) :shift-lock)
        (and (= ch 6) (= (first @current-alphabet) :a2)) (multi-zchar)
        :else (lookup-in-alphabet ch)))
(defmethod-for-values process-zchar '(3 4 5 6 7 8) [ch]
  (cond (<= 1 ch 3) (abbreviation ch (next-zchar))
        (= ch 4) (set-alphabet :a1 :shift)
        (= ch 5) (set-alphabet :a2 :shift)
        (and (= ch 6) (= (first @current-alphabet) :a2)) (multi-zchar)
        :else (lookup-in-alphabet ch)))

(defn read-string
  ([address] (read-string address -1))
  ([address length]
   (dosync (set-alphabet :a0 nil)
           (reset! text-address address)
           (reset! text-end false)
           (reset! read-zchars nil)
           (reset! num-zwords length))
   (loop [chars []
          zch (next-zchar)]
     (if zch
       (let [ch (process-zchar zch)]
         (if ch
           (recur (conj chars ch) (next-zchar))
           (recur chars (next-zchar))))
       (apply str chars)))))

(defn read-string-from [address-ref]
  (let [str (read-string address-ref)]
    (reset! text-address address-ref)
    str))
