(ns wb-es-indexer.db-test
  (:require [clojure.test :refer :all]
            [datomic.api :as d]
            [mount.core :as mount]
            [wb-es-indexer.db :refer :all]))

(defn db-test-fixture [f]
  (mount/start)
  (def db (d/db datomic-conn))
  (f)
  (ns-unmap *ns* 'db)
  (mount/stop))

;; Here we register db-test-fixture to be called once, wrapping ALL tests
;; in the namespace
(use-fixtures :once db-test-fixture)

(deftest test-component-attrs
  (testing "get component attrs based on attr on parent type"
    (is (= (component-attrs db :gene/corresponding-cds)
           [:gene.corresponding-cds/cds]))))

(deftest test-component-attr-of
  (testing "get the parent attr that connects a component"
    (is (= (component-attr-of db :gene.corresponding-cds/cds)
           :gene/corresponding-cds))))

(deftest test-forward-pull-spec
  (testing "forward non-ref attribute"
    (is (= (forward-pull-spec db :gene/public-name)
           [:gene/public-name])))
  (testing "forward ref attribute"
    (is (= (forward-pull-spec db :gene/strain)
           [{:gene/strain [:strain/id]}])))
  (testing "forward ref attribute on component"
    (is (= (forward-pull-spec db :gene/corresponding-cds)
           [{:gene/corresponding-cds [{:gene.corresponding-cds/cds [:cds/id]}]}]))))

(deftest test-reverse-pull-spec
  (testing "reverse ref of strain on gene"
    (is (= (reverse-pull-spec db :gene/strain)
           [{:gene/_strain [:gene/id]}])))
  (testing "reverse ref of gene on interaction"
    (is (= (reverse-pull-spec db :interaction.interactor-overlapping-gene/gene)
           [{:interaction.interactor-overlapping-gene/_gene
             [{:interaction/_interactor-overlapping-gene [:interaction/id]}]}]))))
