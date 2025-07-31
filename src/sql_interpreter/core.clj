(ns sql-interpreter.core
  (:require [sql-interpreter.data :as data]
            [sql-interpreter.query :refer [sql-query]]
            [clojure.string :as str]))


(defn -main []
  (println "Clojure SQL Interpreter (type 'exit' to quit)")
  (println "Loading CSV files...")

  (let [resource-dir "resources"
        csv-files (->> (file-seq (clojure.java.io/file resource-dir))
                       (filter #(and (.isFile %)
                                     (.endsWith (.getName %) ".csv")))
                       (map #(.getName %)))
        data-map (into {}
                       (for [fname csv-files]
                         [fname (try
                                  (data/load-csv-as-maps (str resource-dir "/" fname))
                                  (catch Exception e
                                    (println "Failed to load" fname ":" (.getMessage e))
                                    []))]))]
    (println "Loaded files:" (keys data-map))
    (loop []
      (print "sql> ")
      (flush)
      (let [input (read-line)]
        (if (or (nil? input) (= (str/lower-case input) "exit"))
          (println "Goodbye!")
          (do
            (try
              (let [result (sql-query input data-map)]
                (doseq [row result]
                  (println row)))
              (catch Exception e
                (println "Error:" (.getMessage e))))
            (recur)))))))

