(ns sql-interpreter.query
  (:require [sql-interpreter.parser :refer [parse-where-clause]]
            [clojure.string :as str]))


(defn sql-query [query data-map]
  (let [[_ distinct? select-str file-name where-clause order-by-str limit-str]
        (re-matches
         #"(?i)SELECT\s+(DISTINCT\s+)?(.+?)\s+FROM\s+([\w\.\-_]+)(?:\s+WHERE\s+(.+?))?(?:\s+ORDER\s+BY\s+(.+?))?(?:\s+LIMIT\s+(\d+))?"
         query)]

    (when-not (and select-str file-name)
      (throw (Exception. (str "Invalid SQL query format: " query))))

    (let [file (let [f (str/lower-case file-name)]
                 (if (str/ends-with? f ".csv") f (str f ".csv")))
          table-data (get data-map file)]

      (when-not table-data
        (throw (Exception. (str "File not loaded or found: " file))))

      (let [selected-cols
            (if (= (str/trim select-str) "*")
              (if (seq table-data)
                (keys (first table-data))
                (throw (Exception. "SELECT * is not allowed on empty table.")))
              (map keyword (map str/trim (str/split select-str #","))))

            where-fn (when where-clause (parse-where-clause where-clause))
            filtered-data (if where-fn (filter where-fn table-data) table-data)

            ordered-data
            (if order-by-str
              (let [order-clauses (map str/trim (str/split order-by-str #","))]
                (sort-by
                 (apply juxt
                        (map (fn [clause]
                               (let [[col direction] (map str/trim (str/split clause #"\s+"))
                                     k (keyword col)]
                                 (fn [row] (get row k))))
                             order-clauses))
                 (if (some #(re-find #"(?i)\sdesc$" %) order-clauses)
                   #(compare %2 %1)
                   compare)
                 filtered-data))
              filtered-data)

            limited-data (if limit-str (take (Integer/parseInt limit-str) ordered-data) ordered-data)
            distinct-data (if distinct? (distinct limited-data) limited-data)
            selected-data (map #(select-keys % selected-cols) distinct-data)]

        selected-data))))
