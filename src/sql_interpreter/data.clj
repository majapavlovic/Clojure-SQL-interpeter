(ns sql-interpreter.data
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn load-csv-as-maps [file-path]
  (with-open [reader (io/reader file-path)]
    (let [rows (csv/read-csv reader)
          headers (map keyword (first rows))
          data (rest rows)]
      (doall
       (map (fn [row]
              (into {} (map vector headers row)))
            data)))))
