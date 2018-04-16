(ns nutrimat.util)

(defmacro defmethod-for-values
  [multifn dispatch-values-seq & fn-tail]
  `(doseq [dispatch-val# ~dispatch-values-seq]
     (defmethod ~multifn dispatch-val#
       ~@fn-tail)))

(defn error [& args]
  (throw (Exception. (apply str args))))

(defn format [format-spec & items]
  (. String format format-spec (to-array items)))

(defn system [command-str]
  (let [proc (.. Runtime (getRuntime) (exec command-str))
        output (java.io.BufferedReader.
                (java.io.InputStreamReader. (.getInputStream proc)))]
    (. proc (waitFor))
    (loop [lines []]
      (let [line (.readLine output)]
        (if (not (nil? line))
          (recur (conj lines line))
          lines)))))

(defn hex [n] (format "$%x" n))
