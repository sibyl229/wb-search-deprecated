;; Public_name
;; species

(ns wb-es.datomic.data.variation
  (:require [datomic.api :as d]
            [wb-es.datomic.data.util :as data-util]))

(deftype Variation [entity]
  data-util/Document
  (metadata [this] (data-util/default-metadata entity))
  (data [this]
    {:wbid (:variation/id entity)
     :label (:variation/public-name entity)
     :species (data-util/format-species-enum (:variation/species entity))}))
