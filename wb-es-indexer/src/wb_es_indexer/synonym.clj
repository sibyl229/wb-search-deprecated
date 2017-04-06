(ns wb-es-indexer.synonym
  (:require [clojure.core.async :as async :refer [go chan >! <! close!]]
            [datomic.api :as d]
            [wb-es-indexer.db :refer [datomic-conn pull-spec]]))

  (def synonym-file-path "resources/synonym.txt")

(defn- write-to-synonym-file [synonym-strings]
  )

(defn run
  ([entity-ids batch-synonym-producer] (run entity-ids batch-synonym-producer 10))
  ([entity-ids batch-synonym-producer buffer-size]
   (let [entity-id-batches (partition 50 50 nil entity-ids)
         buffer (chan buffer-size)]
     (go (doseq [batch-entity-ids entity-id-batches]
           (>! buffer (batch-synonym-producer batch-entity-ids))
           (println "push")
           )
         (println "done reading")
         (close! buffer))
     (go
       (with-open [w (clojure.java.io/writer synonym-file-path :append true)]
         (loop []
           (if-let [batch (<! buffer)]
             ;; normal batches won't be nil
             ;; only get nil when channel is closed
             (do
               (println "pop")
               (.write w (clojure.string/join "\n" batch))
               (recur))
             )))))))

(defn get-docs-by-type [db type]
  (d/q '[:find [?e ...]
         :in $ ?ident
         :where
         [?e ?ident]]
       db (keyword type "id")))

(defn get-components-by-entities [db component-attrs entity-ids]
  ;; (d/q '[:find [?cid ...]
  ;;        :in $ [?c-attr ...] [?eid ...]
  ;;        :where
  ;;        [?eid ?c-attr ?cid]]
  ;;      db component-attrs entity-ids)
  (->> (for [c-attr component-attrs
             eid entity-ids]
         (d/datoms db :eavt eid c-attr))
       (apply concat)
       (map (fn [datom]
              [(:e datom) (:v datom)]))))

(defn get-component-attrs-by-type [db type]
  (d/q '[:find [?c-attr ...]
         :in $ ?ns
         :where
         [_ :db.install/attribute ?a]
         [?a :db/isComponent]
         [?a :db/ident ?c-attr]
         [(namespace ?c-attr) ?ns]]
       db type))

(defn run-all []
  (let [db (d/db datomic-conn)
        gene-component-attrs (get-component-attrs-by-type db "gene")]
    (do
      (run
        (take 100000 (drop 31900 (get-docs-by-type db "gene")))
        (fn [entity-ids]
          (->> (get-components-by-entities db gene-component-attrs entity-ids)
               (map (fn [[entity-id component-id]]
                      (format "%s => %s" component-id entity-id)))))))))
