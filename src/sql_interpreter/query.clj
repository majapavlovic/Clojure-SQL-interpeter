(ns sql-interpreter.query
  (:require [sql-interpreter.parser :refer [parse-where-clause]]
            [clojure.string :as str]))

(defn- split-and-clauses
  [s]
  (->> (str/split s #"(?i)\s+AND\s+")
       (map str/trim)
       (remove str/blank?)))

(defn- split-or-clauses
  [s]
  (->> (str/split s #"(?i)\s+OR\s+")
       (map str/trim)
       (remove str/blank?)))

(defn- build-and-predicate
  [and-group]
  (let [parts (split-and-clauses and-group)]
    (cond
      (empty? parts) (constantly true)
      (= 1 (count parts)) (parse-where-clause (first parts))
      :else (apply every-pred (map parse-where-clause parts)))))

(defn- build-where-fn
  [where-clause]
  (let [or-groups (split-or-clauses where-clause)]
    (cond
      (empty? or-groups) nil
      (= 1 (count or-groups)) (build-and-predicate (first or-groups))
      :else
      (let [preds (map build-and-predicate or-groups)]
        (fn [row] (some #(true? (% row)) preds))))))

(defn- parse-order-by
  [order-by-str]
  (->> (str/split order-by-str #",")
       (map str/trim)
       (remove str/blank?)
       (map (fn [clause]
              (let [[col dir] (map str/trim (str/split clause #"\s+"))
                    k   (keyword col)
                    dir (if (and dir (re-matches #"(?i)desc" dir)) :desc :asc)]
                {:key k :dir dir})))
       vec))

(defn- safe-compare
  [a b]
  (cond
    (and (nil? a) (nil? b)) 0
    (nil? a) -1
    (nil? b)  1
    :else (compare a b)))

(defn- build-order-comparator
  [specs]
  (fn [x y]
    (loop [[{:keys [key dir]} & more] specs]
      (if key
        (let [c (safe-compare (get x key) (get y key))
              c (if (= dir :desc) (- c) c)]
          (if (zero? c) (recur more) c))
        0))))

(defn- parse-select-cols
  [select-str first-row]
  (if (= (str/trim select-str) "*")
    (if (seq first-row)
      (vec (keys first-row))
      (throw (Exception. "SELECT * is not allowed on empty table.")))
    (->> (str/split select-str #",")
         (map str/trim)
         (remove str/blank?)
         (map keyword)
         vec)))

(defn sql-query [query data-map]
  (let [[_ distinct? select-str file-name where-clause order-by-str limit-str]
        (re-matches
         #"(?i)SELECT\s+(DISTINCT\s+)?(.+?)\s+FROM\s+([\w.\-_]+)(?:\s+WHERE\s+(.+?))?(?=\s+ORDER\s+BY|\s+LIMIT|$)(?:\s+ORDER\s+BY\s+(.+?))?(?:\s+LIMIT\s+(\d+))?"
         (str/trim query))]

    (when-not (and select-str file-name)
      (throw (Exception. (str "Invalid SQL query format: " query))))

    (let [file (let [f (str/lower-case file-name)]
                 (if (str/ends-with? f ".csv") f (str f ".csv")))
          table-data (get data-map file)]

      (when-not table-data
        (throw (Exception. (str "File not loaded or found: " file))))

      (let [selected-cols (parse-select-cols select-str (first table-data))
            where-fn (when where-clause (build-where-fn where-clause))
            filtered-data (if where-fn (filter where-fn table-data)
                              table-data)
            ordered-data
            (if (and order-by-str (not (str/blank? order-by-str)))
              (let [specs (parse-order-by order-by-str)]
                (sort (build-order-comparator specs) filtered-data))
              filtered-data)

            limited-data (if limit-str
                           (take (Integer/parseInt limit-str) ordered-data)
                           ordered-data)

            distinct-data (if distinct?
                            (distinct limited-data) limited-data)

            projected (map #(select-keys % selected-cols) distinct-data)]

        projected))))
