(ns wb-es.mappings.core)

(def default-mapping
  {:properties
   {:wbid {:type "string"
           :index "not_analyzed"}
    :label {:type "string"
            :fields {:raw {:type "string"
                           :analyzer "keyword_ignore_case"}
                     :autocomplete {:type "string"
                                    :analyzer "autocomplete"
                                    :search_analyzer "standard"}}
            }}})

(def index-settings
  {:settings
   {:number_of_shards 1
    :analysis {:filter {"autocomplete_filter" {:type "edge_ngram"
                                               :min_gram 2
                                               :max_gram 20}}
               :analyzer {"autocomplete" {:type "custom"
                                          :tokenizer "standard"
                                          :filter ["lowercase" "autocomplete_filter"]}
                          "keyword_ignore_case" {:type "custom"
                                                 :tokenizer "keyword"
                                                 :filter ["lowercase"]}}}}
   :mappings {:_default_ default-mapping}})
