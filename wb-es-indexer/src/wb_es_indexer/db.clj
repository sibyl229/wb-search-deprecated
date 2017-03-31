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


;; datomic schema queries
;; (defn core-type-idents [db]
;;   (d/q '[:find [?ident ...]
;;          :in $ ?ident-name
;;          :where
;;          [?e :db/ident ?ident]
;;          [_ :db.install/attribute ?e]
;;          [(name ?ident) ?ident-name]
;;          [(namespace ?ident) ?ns]
;;          (not-join [?ns]
;;                    [_ :pace/use-ns ?ns])]
;;        db "id"))
(def core-type-names
  ["analysis"
   "antibody"
   "cds"
   "clone"
   "construct"
   "disease"
   "feature"
   "gene"
   "gene-class"
   "go-term"
   "laboratory"
   "life-stage"
   "molecule"
   "motif"
   "operon"
   "paper"
   "pcr-oligo"
   "person"
   "phenotype"
   "picture"
   "protein"
   "pseudogene"
   "rnai"
   "sequence"
   "species"
   "strain"
   "transcript"
   "transgene"
   "transposon"
   "variation"
   "wbprocess"])

(defn get-reverse-ref-attrs [db type-name]
  (let [idents
        (d/q '[:find [?ident ...]
               :in $ ?ref-ident
               :where
               [?e :pace/obj-ref ?ref-ident]
               [?e :db/ident ?ident]
               [_ :db.install/attribute ?e]]
             db (keyword type-name "id"))]
    idents))

(defn is-ns-attr [db attr]
  (d/q '[:find [?ident ...]
         :in $
         :where
         [?e :pace/use-ns ?ns]
         [?e :db/ident ?ident]
         [_ :db.install/attribute ?e]]
       db (namespace attr)))

(defn component-attrs [db attr]
  (let [component-name (str (namespace attr)
                            "."
                            (name attr))]
    (seq
     (d/q '[:find [?ident ...]
            :in $ ?cn
            :where
            [?e :db/ident ?ident]
            [(namespace ?ident) ?cn]
            [_ :db.install/attribute ?e]]
          db component-name))))

(defn component-attr-of [db component-attr]
  (let [name-parts (clojure.string/split (namespace component-attr) #"\.")]
    (if (= (count name-parts) 2)
      (let [parent-attr (keyword (clojure.string/join "/" name-parts))]
        (if (d/entity db [:db/ident parent-attr])
          parent-attr)))))

(defn- reverse-attr [attr]
  (keyword (str (namespace attr)
                "/_"
                (name attr))))

;; End datomic schema queries

(defn- remove-from-pull-spec [pull-spec attr]
  (->> pull-spec
       (filter (fn [spec-pattern]
                 (and (not= spec-pattern attr)
                      (or (not (map? spec-pattern))
                          (not (contains? spec-pattern attr))))))
       (apply vector)))


;;
;; pull spec parts
;;
(defn forward-pull-spec [db attr]
  (let [attr-entity (d/entity db [:db/ident attr])
        attr-type (:db/valueType attr-entity)]
    (if (= :db.type/ref attr-type)
      (if-let [ref-ident (:pace/obj-ref attr-entity)]
        ;; object (non-component) ref
        [{attr [ref-ident]}]
        (if-let [c-attrs (component-attrs db attr)]
          ;; component ref
          [{attr (->> c-attrs
                      (map #(forward-pull-spec db %))
                      (reduce concat []))}]
          []))
      [attr])))

(defn reverse-pull-spec [db attr]
  (let [attr-entity (d/entity db [:db/ident attr])]
    (if-let [parent-attr (component-attr-of db attr)]
      [{(reverse-attr attr) [{(reverse-attr parent-attr)
                              [(keyword (namespace parent-attr) "id")]}]}]
      [{(reverse-attr attr) [(keyword (namespace attr) "id")]}])))
;;
;; end of pull spec parts
;;
