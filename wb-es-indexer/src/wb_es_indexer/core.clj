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

;; (defn- create-batch [dbids doc-factory]
;;   (->> (map doc-factory dbids)
;;        (map generate-batch-style-doc)
;;        (clojure.string/join "")))

(defn create-batch [db pull-spec batch-dbids]
  (->> batch-dbids
       (d/pull-many db pull-spec)
       (map format-doc)
       (map generate-batch-style-doc)
       (clojure.string/join "")))

(defn generate-batches [batch-size doc-dbids]
  (->> (partition batch-size batch-size nil doc-dbids)
       ))

(defn index-batch [batch]
  (elasticsearch "_bulk"))

(defn format-doc [doc]
  {:type "gene"
   :doc (fix-field-names doc)})

(defn run
  ([doc-dbids] (run doc-dbids (pull-spec (d/db datomic-conn) type) 10))
  ([doc-dbids pull-pattern] (run doc-dbids pull-pattern 10))
  ([doc-dbids pull-pattern buffer-size]
   (let [doc-batches (generate-batches 10 doc-dbids)
         buffer (chan buffer-size)
         db (d/db datomic-conn)]
     (go (doseq [batch-dbids doc-batches]
           ;;(>! buffer (create-batch batch-dbids doc-factory))
           (>! buffer (create-batch db pull-pattern batch-dbids))
           (println "push")
           )
         (println "done reading")
         (close! buffer))
     (go (loop []
           (if-let [batch (<! buffer)]
             ;; normal batches won't be nil
             ;; only get nil when channel is closed
             (do
               (println "pop")
               (elasticsearch "_bulk" batch)
               (recur))
             ))))))

(defn get-docs-by-type
  ([db type]
   (->> (d/q '[:find [?e ...]
               :in $ ?ident
               :where
               [?e ?ident]]
             db (keyword type "id")))))

(defn- get-doc [db entity-id pull-pattern]
  (assoc {}
         :id entity-id
         :type "gene"
         :doc (fix-field-names (d/pull db pull-pattern entity-id))))

(defn run-all []
  (let [db (d/db datomic-conn)]
    (do
      (run
        (take 100 (drop 30000 (get-docs-by-type db "gene")))
        (pull-spec db "gene" [:gene/rnaseq :gene/ortholog :gene/paralog :gene/other-sequence])))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
