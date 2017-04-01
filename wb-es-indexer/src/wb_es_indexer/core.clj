(ns wb-es-indexer.core
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.core.async :as async :refer [go chan >! <! close!]]
            [clojure.walk :as walk]
            [datomic.api :as d]
            [mount.core :as mount]
            [wb-es-indexer.db :refer [datomic-conn pull-spec]]))


(def es-base-url "http://localhost:9200")
(def es-index-name "ws259")


(defn elasticsearch [path body]
  (let [es-url (clojure.string/join "/" [es-base-url es-index-name path])]
    (if (re-find #"_bulk" path)
      (http/put es-url {:accept :json
                        :content-type :x-ndjson
                        :body body})
      (http/put es-url {:accept :json
                        :content-type :json
                        :body (json/encode body)}))))

(defn format-field-name [field-name]
  (let [string-field-name (if (keyword? field-name)
                            (second (re-matches #":((.*\/)?.*)" (str field-name)))
                            field-name)]
    (clojure.string/replace string-field-name #"\." "\\$")))

(defn fix-field-names
  "Recursively convert all map keys to elasticsearch compatible field names"
  [doc]
  (let [f (fn [[k v]] [(format-field-name k) v])]
    ;; only apply to maps
    (walk/postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) doc)))
(defn- generate-batch-style-doc [doc]
  (let [action {:index {:_id (:id doc)
                        :_type (:type doc)}}
        body (:doc doc)]
    (str (json/generate-string action)
         "\n"
         (json/generate-string body)
         "\n")))

(defn generate-batches [batch-size docs]
  (->> (map generate-batch-style-doc docs)
       (partition batch-size batch-size nil)
       (map (fn [batched-docs]
              (clojure.string/join "" batched-docs)))))

(defn index-batch [batch]
  (elasticsearch "_bulk"))

(defn run
  ([docs] (run docs 10))
  ([docs buffer-size]
   (let [doc-batches (generate-batches 1000 docs)
         buffer (chan buffer-size)]
     (go (doseq [batch doc-batches]
           (>! buffer batch))
         (close! buffer))
     (go (loop []
           (if-let [batch (<! buffer)]
             ;; normal batches won't be nil
             ;; only get nil when channel is closed
             (do
               (elasticsearch "_bulk" batch)
               (recur))))))))

(defn get-docs-by-type
  ([db type]
   (get-docs-by-type db type (pull-spec db type)))
  ([db type pull-spec]
   (->> (d/q '[:find [?e ...]
               :in $ ?ident
               :where
               [?e ?ident]]
             db (keyword type "id"))
        (map (fn [entity-id]
               (assoc {}
                      :id entity-id
                      :type type
                      :doc (fix-field-names (d/pull db pull-spec entity-id))))))))

(defn run-all []
  (do

    (run (take 2 (get-docs-by-type db "gene"
                                   (pull-spec db "gene" [:gene/rnaseq :gene/ortholog :gene/other-sequence]))))
    (run (lazy-seq '({:id 1 :type "gene" :doc {:a 1 :b 2}} {:id 2 :type "gene" :doc {"a$b" 1 :b 2}} {:id 4 :type "gene" :doc {:a 3333333}} {:id 5 :type "gene" :doc {:a 1 :b 2}}))))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
