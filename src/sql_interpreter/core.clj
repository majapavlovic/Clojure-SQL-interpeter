(ns sql-interpreter.core
  (:require [sql-interpreter.data :as data]
            [clojure.string :as str]))


(defn parse-where-clause [clause]
  (let [[field op & rest] (str/split clause #"\s+")
        value-str (str/join " " rest)
        field-key (keyword field)]
    (cond
      (= op "IN")
      (let [values-str (str/trim value-str)
            cleaned (-> values-str
                        (str/replace #"^\(" "")
                        (str/replace #"\)$" ""))
            values (->> (str/split cleaned #",")
                        (map str/trim)
                        (map #(str/replace % #"^['\"]|['\"]$" ""))
                        (map #(try (Integer/parseInt %) (catch Exception _ %))))
            value-set (set values)]
        (fn [row]
          (contains? value-set (get row field-key))))

      (= op "BETWEEN")
      (let [[v1 _ v2] (str/split value-str #"\s+")
            lower (try (Integer/parseInt v1) (catch Exception _ nil))
            upper (try (Integer/parseInt v2) (catch Exception _ nil))]
        (if (and lower upper)
          (fn [row]
            (let [v (get row field-key)]
              (and (number? v) (<= lower v upper))))
          (throw (Exception. (str "Invalid BETWEEN values: " value-str)))))

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
                     (throw (Exception. (str "Unsupported operator: " op))))]
        (eval
         `(fn [~'row]
            (let [field-val# (get ~'row ~field-key)]
              (when (and (some? field-val#)
                         (or (and (number? field-val#) (number? ~value))
                             (and (string? field-val#) (string? ~value))))
                (~op-sym field-val# ~value)))))))))




(defn sql-query [query data-map]
  (let [[_ select-str file-name where-clause]
        (re-matches #"(?i)SELECT\s+(.+?)\s+FROM\s+([\w\.\-_]+)(?:\s+WHERE\s+(.+))?" query)]
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
            filtered-data (if where-fn
                            (filter where-fn table-data)
                            table-data)]

        (map #(select-keys % selected-cols) filtered-data)))))





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

