(ns wb-es-indexer.db-test
  (:require [clojure.test :refer :all]
            [mount.core :as mount]
            [wb-es-indexer.db :refer :all]))

(defn db-test-fixture [f]
  (mount/start)
  (f)
  (mount/stop))

;; Here we register db-test-fixture to be called once, wrapping ALL tests
;; in the namespace
(use-fixtures :once db-test-fixture)

(deftest test-pull-spec
  (testing "reverse ref of gene on interaction"
    (= (reverse-spec "gene" :interaction.interactor-overlapping-gene/gene)
       {:interaction.interactor-overlapping-gene/_gene
        [{:interaction/_interactor-overlapping-gene [:interaction/id]}]})))
