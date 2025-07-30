(ns sql-interpreter.core
  (:require [sql-interpreter.data :as data]
            [clojure.string :as str]))


(defn parse-where-clause [clause]
  (let [[field op value-str] (str/split clause #"\s+")
        field-key (keyword field)
        value (try (Integer/parseInt value-str)
                   (catch Exception _ value-str))
        op-sym (case op
                 "=" `=
                 "!=" `not=
                 ">" `>
                 "<" `<
                 ">=" `>=
                 "<=" `<=
                 (throw (Exception. (str "Wrong operator: " op))))]
    (eval
     `(fn [~'row] (~op-sym (get ~'row ~field-key) ~value)))))


(defn sql-query [query table-data]
  (let [[_ select-str where-clause] (re-matches #"(?i)SELECT (.+) FROM \w+(?: WHERE (.+))?" query)
        selected-cols (map keyword (map str/trim (str/split select-str #",")))
        where-fn (when where-clause (parse-where-clause where-clause))
        filtered-data (if where-fn
                        (filter where-fn table-data)
                        table-data)]
    (map #(select-keys % selected-cols) filtered-data)))



(defn -main []
  (let [table (data/load-csv-as-maps "resources/data.csv")
        query "SELECT one, two FROM data WHERE four > 2"
        result (sql-query query table)]
    (doseq [row result]
      (println row))))
