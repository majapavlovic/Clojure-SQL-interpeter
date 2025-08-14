(ns sql-interpreter.core-test
  (:require [clojure.test :refer :all]
            [sql-interpreter.query :refer [sql-query]]
            [sql-interpreter.data :as data]))

(def test-data-map
  {"client.csv" (data/load-csv-as-maps "resources/client.csv")
   "contract.csv" (data/load-csv-as-maps "resources/contract.csv")})

(deftest test-select-all
  (let [result (sql-query "SELECT * FROM client" test-data-map)]
    (is (= 12 (count result)))))

(deftest test-where-clause
  (let [result (sql-query "SELECT name FROM client WHERE age > 40" test-data-map)]
    (is (= #{"Nemanja" "Nikola"} (set (map :name result))))))

(deftest test-order-by-asc
  (let [result (sql-query "SELECT name FROM client ORDER BY age ASC" test-data-map)]
    (is (= ["Jelena" "Ana" "Tamara" "Milica" "Petar" "Marko" "Luka" "Ivan" "Stefan" "Marija" "Nemanja" "Nikola"]
           (map :name result)))))

(deftest test-order-by-desc
  (let [result (sql-query "SELECT name FROM client ORDER BY age DESC LIMIT 3" test-data-map)]
    (is (= ["Nemanja" "Nikola" "Stefan"] (map :name result)))))

(deftest test-limit
  (let [result (sql-query "SELECT name FROM client LIMIT 2" test-data-map)]
    (is (= 2 (count result)))))

(deftest test-distinct
  (let [result (sql-query "SELECT DISTINCT age FROM client" test-data-map)]
    (is (= #{28 31 34 39 45} (set (map :age result))))))

(deftest test-where-order-limit-combined
  (let [result (sql-query "SELECT name FROM client WHERE age > 30 ORDER BY age DESC LIMIT 2" test-data-map)]
    (is (= ["Nemanja" "Nikola"] (map :name result)))))

(deftest test-where-and
  (let [result (sql-query "SELECT name FROM client WHERE age >= 39 AND contact_email LIKE '%@gmail.com'" test-data-map)]
    (is (= ["Nikola"] (map :name result)))))

(deftest test-or
  (let [result (sql-query "SELECT name FROM client WHERE age >= 39 OR contact_phone LIKE '60%'" test-data-map)]
    (is (= ["Marko" "Nemanja" "Stefan" "Nikola" "Marija" "Tamara"] (map :name result)))))

(deftest test-or-orderby-limit
  (let [result (sql-query "SELECT id, name FROM client WHERE age > 31 or contact_phone LIKE '61%' ORDER BY id DESC LIMIT 2" test-data-map)]
    (is (= #{{:id 12 :name "Ivan"}
             {:id 10 :name "Marija"}}))))

(deftest order-by-combined
  (let [result (sql-query "SELECT contractid, contract_obligation, subscription FROM contract ORDER BY contract_obligation ASC, subscription DESC" test-data-map)]
    (is (= #{{:contractid 104, :contract_obligation "12 months", :subscription 3900}
             {:contractid 102, :contract_obligation "12 months", :subscription 1490}
             {:contractid 105, :contract_obligation "18 months", :subscription 2490}
             {:contractid 101, :contract_obligation "24 months", :subscription 4990}
             {:contractid 103, :contract_obligation "24 months", :subscription 2590}}))))