(ns nutrimat.zmachine
  (:require [nutrimat.binary :as binary]
            [nutrimat.util :as util :refer [defmethod-for-values]]
            [nutrimat.zstory :as zstory]
            [nutrimat.zmem :as zmem]))

;; Frame Stack
(defn current-frame []
  (first (:frames @zstory/story)))

(defn frame-pop []
  (let [frame (first (:frames @zstory/story))]
    (when-not frame
      (util/error "Frame stack underflow"))
    (swap! zstory/story update-in [:frames] rest)
    frame))

(defn frame-push [frame]
  (swap! zstory/story update-in [:frames] conj frame))

;; Stack
(defn stack-pop []
  (let [value (first (:stack @zstory/frame))]
    (when-not value
      (util/error "Game stack underflow"))
    (swap! zstory/frame update-in [:stack] rest)
    value))

(defn stack-push [value]
  (swap! zstory/frame update-in [:stack] conj value))

;; PC
(defn pc []
  (:pc @zstory/frame))

(defn set-pc [addr]
  (swap! zstory/frame assoc :pc addr))

(defn inc-pc
  ([] (set-pc (inc (pc))))
  ([x] (set-pc (+ (pc) x))))

(defn next-byte []
  (let [byte (zmem/zbyte (pc))]
    (inc-pc)
    byte))

(defn next-word []
  (let [word (zmem/zword (pc))]
    (inc-pc 2)
    word))

;; Breakpoints
(defn at-breakpoint? [addr]
  (contains? (:breakpoints @zstory/story) addr))

(defn set-breakpoint [addr]
  (swap! zstory/story update-in [:breakpoints] conj addr))

(defn clear-breakpoint [addr]
  (swap! zstory/story update-in [:breakpoints] disj addr))

(defn clear-breakpoints []
  (swap! zstory/story assoc :breakpoints #{}))

;; Global variables
(defn get-global-variable [var]
  (let [table (zmem/zword zmem/header-global-variable-table)]
    (zmem/zword (+ table (* var 2)))))

(defn set-global-variable [var value]
  (let [table (zmem/zword zmem/header-global-variable-table)]
    (zmem/set-zword (+ table (* var 2)) value)))

;; Local variables
(defn num-locals []
  (count (:locals @zstory/frame)))

(defn local [n]
  (and (contains? (:locals @zstory/frame) n)
       (nth (:locals @zstory/frame) n)))

(defn set-local [n value]
  (swap! zstory/frame assoc-in [:locals] n value))

(defn get-locals []
  (:locals @zstory/frame))

(defn set-locals [locals]
  (swap! zstory/frame assoc :locals locals))

(defmulti read-locals zmem/version)
(defmethod-for-values read-locals '(1 2 3 4) [num-locals]
  (set-locals
   (loop [locals []
          num num-locals]
     (if (> num 0)
       (recur (conj locals (next-word)) (dec num))
       locals))))
(defmethod read-locals :default [num-locals]
  (set-locals (if (zero? num-locals)
                nil
                (vec (repeat num-locals 0)))))

;; Variables
(defn get-variable [var]
  (cond (= var 0x0) (stack-pop)
        (<= 0x1 var 0xf) (local (dec var))
        :else (get-global-variable (- var 0x10))))

(defn set-variable [var value]
  (cond (= var 0x0) (stack-push value)
        (<= 0x1 var 0xf) (set-local (dec var) value)
        :else (set-global-variable (- var 0x10) value)))

(defn inc-variable [var]
  (set-variable var (inc (binary/signed-word (get-variable var)))))

(defn dec-variable [var]
  (set-variable var (dec (binary/signed-word (get-variable var)))))

;; Initial frame
(defmulti push-initial-frame zmem/version)
(defmethod push-initial-frame :default []
  (let [start-pc (zmem/zword zmem/header-main-address)]
    (frame-push (zstory/create-frame start-pc))))
(defmethod push-initial-frame 6 []
  (let [start-pc (zmem/routine-address (zmem/zword zmem/header-main-address))
        num-locals (zmem/zbyte start-pc)]
    (frame-push (zstory/create-frame (inc start-pc)))
    (read-locals num-locals)))

(defn merge-locals-with-operands [locals operands]
  (let [lc (count locals)
        oc (count operands)]
    (vec (if (<= lc oc)
           (take lc operands)
           (concat operands (subvec locals oc))))))

;; Routine calls
(defn call-routine [routine-address operands result]
  (cond (zero? routine-address) (when result (set-variable result 0))
        :else
        (dosync
         (swap! zstory/frame assoc :result-store result)
         (frame-push (zstory/create-frame routine-address))
         (read-locals (next-byte))
         (set-locals (merge-locals-with-operands (get-locals) operands)))))

(defn routine-return [result]
  (dosync
   (when (= (count (:frames @zstory/story)) 1)
     (util/error "Illegal return from main routine"))
   (frame-pop)
   (let [result-store (:result-store @zstory/story)]
     (when result-store
       (set-variable result-store result)))))

(defn return-true [] (routine-return 1))
(defn return-false [] (routine-return 0))

(defn jump-if [value branch-info]
  (if (= value (first branch-info))
    (let [address (second branch-info)]
      (cond (= address :ret-true) (return-true)
            (= address :ret-false) (return-false)
            :else (set-pc address)))))
