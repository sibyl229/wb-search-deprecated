;; id
;; species

(ns wb-es.datomic.data.transcript
  (:require [datomic.api :as d]
            [wb-es.datomic.data.util :as data-util]))

(deftype Transcript [entity]
  data-util/Document
  (metadata [this] (data-util/default-metadata entity))
  (data [this]
    {:wbid (:transcript/id entity)
     :label (:transcript/id entity)
     :species (data-util/format-entity-species :transcript/species entity)}))
