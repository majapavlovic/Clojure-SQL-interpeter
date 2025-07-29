(ns sql-interpreter.core
  (:require [sql-interpreter.data :as data]
            [clojure.string :as str]))


(defn sql-query [query table-data]
  (let [[_ select-str where-clause] (re-matches #"(?i)SELECT (.+) FROM \w+(?: WHERE (.+))?" query)
        selected-cols (map keyword (map str/trim (str/split select-str #",")))]
    (map #(select-keys % selected-cols) table-data)))


(defn -main []
  (let [table (data/load-csv-as-maps "resources/data.csv")
        query "SELECT one, two FROM data"
        result (sql-query query table)]
    (doseq [row result]
      (println row))))