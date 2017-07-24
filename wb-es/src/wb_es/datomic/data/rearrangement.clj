;; id


(ns wb-es.datomic.data.rearrangement
  (:require [datomic.api :as d]
            [wb-es.datomic.data.util :as data-util]))

(deftype Rearrangement [entity]
  data-util/Document
  (metadata [this] (data-util/default-metadata entity))
  (data [this]
    {:wbid (:rearrangement/id entity)
     :wbid_as_label (:rearrangement/id entity)
     :species (data-util/format-species-enum (:rearrangement/species entity))}))