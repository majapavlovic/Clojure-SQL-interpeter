(ns sql-interpreter.core
  (:require [sql-interpreter.data :as data]
            [clojure.string :as str]))


(defn parse-where-clause [clause]
  (let [[field op value-str] (str/split clause #"\s+")
        field-key (keyword field)]
    (cond
      (= op "LIKE")
      (let [pattern (str/trim value-str)
            unquoted (if (re-matches #"^['\"].*['\"]$" pattern)
                       (subs pattern 1 (dec (count pattern)))
                       pattern)]
        (cond
          (and (str/starts-with? unquoted "%")
               (str/ends-with? unquoted "%"))
          (fn [row]
            (str/includes? (str/lower-case (str (get row field-key)))
                           (str/lower-case (subs unquoted 1 (dec (count unquoted))))))

          (str/starts-with? unquoted "%")
          (fn [row]
            (str/ends-with? (str/lower-case (str (get row field-key)))
                            (str/lower-case (subs unquoted 1))))

          (str/ends-with? unquoted "%")
          (fn [row]
            (str/starts-with? (str/lower-case (str (get row field-key)))
                              (str/lower-case (subs unquoted 0 (dec (count unquoted))))))

          :else
          (fn [row]
            (= (str/lower-case (str (get row field-key)))
               (str/lower-case unquoted)))))

      :else
      (let [value (try (Integer/parseInt value-str)
                       (catch Exception _ (str/replace value-str #"^['\"]|['\"]$" "")))
            op-sym (case op
                     "=" `=
                     "!=" `not=
                     ">" `>
                     "<" `<
                     ">=" `>=
                     "<=" `<=
                     (throw (Exception. (str "Wrong operator: " op))))]
        (eval
         `(fn [~'row] (~op-sym (get ~'row ~field-key) ~value)))))))



(defn sql-query [query table-data]
  (let [[_ select-str where-clause] (re-matches #"(?i)SELECT\s+(.+?)\s+FROM\s+\w+(?:\s+WHERE\s+(.+))?" query)]
    (when-not select-str
      (throw (Exception. (str "Invalid SQL query format: " query))))

    (let [select-str (str/trim select-str)
          selected-cols
          (if (= select-str "*")
            (if (seq table-data)
              (keys (first table-data))
              (throw (Exception. "SELECT * is not allowed on empty table.")))
            (map keyword (map str/trim (str/split select-str #","))))

          where-fn (when where-clause (parse-where-clause where-clause))
          filtered-data (if where-fn
                          (filter where-fn table-data)
                          table-data)]

      (map #(select-keys % selected-cols) filtered-data))))




(defn -main []
  (let [table (data/load-csv-as-maps "resources/data.csv")
        query "SELECT * FROM data WHERE four > 2"
        result (sql-query query table)]
    (doseq [row result]
      (println row))))
