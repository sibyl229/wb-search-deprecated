(ns wb-es-indexer.db
  (:require
   [datomic.api :as d]
   [environ.core :as environ]
   [mount.core :as mount]))

;;
;; database connection
;;
(defn datomic-uri []
  (environ/env :wb-db-uri))

(defn- connect []
  (let [db-uri (datomic-uri)]
    (d/connect db-uri)))

(defn- disconnect [conn]
  (d/release conn))

(mount/defstate datomic-conn
  :start (connect)
  :stop (disconnect datomic-conn))

;;
;; end of database connection
;;


;;
;; pull spec parts
;;
(defn reverse-spec [type-name reverse-ref-name])
;;
;; end of pull spec parts
;;
