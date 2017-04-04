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
  #{"analysis"
    "antibody"
    "cds"
    "clone"
    "construct"
    "disease"
    "expr-pattern"
    "feature"
    "gene"
    "gene-class"
    "go-term"
    "interaction"
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
    "wbprocess"})


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

(defn- remove-from-pull-spec [attrs pull-spec]
  (let [attrs-set (set attrs)]
    (->> pull-spec
         (filter (fn [spec-pattern]
                   (and (not (attrs-set spec-pattern))
                        (or (not (map? spec-pattern))
                            (not (attrs-set (first (keys spec-pattern))))))))
         (apply vector))))


;;
;; pull spec parts
;;
(defn forward-pull-spec [db attr]
  (let [attr-entity (d/entity db [:db/ident attr])
        attr-type (:db/valueType attr-entity)]
    (if (= :db.type/ref attr-type)
      (if-let [ref-ident (:pace/obj-ref attr-entity)]
        ;; object (non-component) ref
        ;;[{attr [ref-ident]}]
        [attr]
        (if-let [c-attrs (component-attrs db attr)]
          ;; component ref
          ;; [{attr (->> c-attrs
          ;;             (map #(forward-pull-spec db %))
          ;;             (reduce concat [])
          ;;             (apply vector))}]
          [attr]
          []))
      [attr])))

(defn reverse-pull-spec [db attr]
  (let [attr-entity (d/entity db [:db/ident attr])]
    (if-let [parent-attr (component-attr-of db attr)]
      (let [parent-ident (keyword (namespace parent-attr) "id")]
        (if (core-type-names (namespace parent-ident))
          ;; [{(reverse-attr attr) [{(reverse-attr parent-attr)
          ;;                         [parent-ident]}]}]
          [{(reverse-attr attr) [(reverse-attr parent-attr)]}]
          []))
      (let [other-ident (keyword (namespace attr) "id")]
        (if (core-type-names (namespace other-ident))
          ;;  [{(reverse-attr attr) [other-ident]}]
          [(reverse-attr attr)]
          [])))))
;;
;; end of pull spec parts
;;


;;
;; pull from a generic type
;;

(defn get-reverse-attrs [db type-name]
  (let [idents
        (d/q '[:find [?ident ...]
               :in $ ?ref-ident
               :where
               [?e :pace/obj-ref ?ref-ident]
               [?e :db/ident ?ident]
               [_ :db.install/attribute ?e]]
             db (keyword type-name "id"))]
    idents))

(defn get-attrs-by-type [db type-name]
  (d/q '[:find [?ident ...]
         :in $ ?ns
         :where
         [?e :db/ident ?ident]
         [(namespace ?ident) ?ns]
         [_ :db.install/attribute ?e]]
       db type-name))

(defn pull-spec
  ([db type-name]
   (pull-spec db type-name []))
  ([db type-name skip-attrs]
   (let [forward-attrs (get-attrs-by-type db type-name)
         reverse-attrs (get-reverse-attrs db type-name)]
     (into [] (concat (->> (reduce (fn [accumulator attr]
                                     (concat accumulator (forward-pull-spec db attr)))
                                   []
                                   forward-attrs)
                           (remove-from-pull-spec skip-attrs))
                      (->> (reduce (fn [accumulator attr]
                                     (concat accumulator (reverse-pull-spec db attr)))
                                   []
                                   reverse-attrs))
                      )))))

;; example
;; (d/pull db (pull-spec db "gene" [:gene/rnaseq :gene/ortholog :gene/other-sequence]) [:gene/id "WBGene00015146"])

;;
;; end pull from a generic type
;;
