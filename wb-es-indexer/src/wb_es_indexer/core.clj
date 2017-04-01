(ns wb-es-indexer.core
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.core.async :as async :refer [go chan >! <! close!]]
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

(defn- generate-batch-style-doc [doc]
  (let [action {:index {:_id (:id doc)
                        :_type (:type doc)}}
        body (:doc doc)]
    (str (json/generate-string action)
         "\n"
         (json/generate-string body))))

(defn generate-batches [batch-size docs]
  (->> (map generate-batch-style-doc docs)
       (partition batch-size batch-size nil)
       (map (fn [batched-docs]
              (str (clojure.string/join "\n" batched-docs)
                   "\n")))
       ))

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
               (println (elasticsearch "_bulk" batch))
               (println "Batch: ")
               (println batch)
               (recur))))))))
(defn run-all []
  (run [{:id 1 :type "gene" :doc {:a 1 :b 2}} {:id 2 :type "gene" :doc {:a 1 :b 2}} {:id 4 :type "gene" :doc {:a 3333333}} {:id 5 :type "gene" :doc {:a 1 :b 2}}]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
