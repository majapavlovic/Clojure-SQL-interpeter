(ns sql-interpreter.core
  (:require [sql-interpreter.data :as data]))

(defn -main []
  (let [data (data/load-csv-as-maps "resources/data.csv")]
    (doseq [row data]
      (println row))))
