(ns wb-es.web.integration)

(defn- pack-search-obj [doc]
  (let [doc-source (:_source doc)]
    {:id (:wbid doc-source)
     :class (clojure.string/replace (:_type doc) "-" "_")
     :label (or (:label doc-source)
                (:wbid doc-source))
     :taxonomy (get-in doc-source [:species :key])}))

(defn- pack-species [species-name]
  (if species-name
    (let [[genus-name species-name] (clojure.string/split species-name #"\s+" 2)]
      {:genus genus-name
       :species species-name})))

(defn- get-obj [doc]
  (let [search-obj (pack-search-obj doc)
        doc-source (:_source doc)]
    {:name search-obj
     :description (:description doc-source)
     :taxonomy (->> (get-in doc-source [:species :name])
                    (pack-species))
     :gene (->> (:gene doc-source)
                (not-empty))
     :phenotype (->> (:phenotype doc-source)
                     (not-empty))
     :genotype (->> (:genotype doc-source)
                    (not-empty))
     :strain (->> (:strain doc-source)
                  (not-empty))
     :author (->> (:author doc-source)
                  (not-empty))}))

(defn wrap-params [handler]
  (fn [request]
    (let [params (:params request)
          page (get params :page 1)
          page-size (get params :page_size 10)
          params-new (assoc params
                            :size page-size
                            :from (->> page
                                       (Integer.)
                                       (dec)
                                       (* page-size))
                            :q (some-> (get params :q (:query params))
                                       (clojure.string/replace #"\*" ""))
                            :type (get params :type (:class params))
                            :species (:species params)
                            :raw params)]
      (handler (assoc request :params params-new)))))

(defn wrap-search [handler]
  (fn [request]
    (let [response (handler request)
          body-new {:matches (->> (get-in response [:body :hits :hits])
                                  (map get-obj))
                    :query (get-in request [:params :q])
                    :querytime (-> (get-in response [:body :took])
                                   (/ 1000.0))
                    :count (get-in response [:body :hits :total])
                    :page (get-in request [:params :raw :page])
                    :page_size (get-in request [:params :raw :page_size])}]
      (assoc response :body body-new))))

(defn wrap-autocomplete [handler]
  (fn [request]
    (let [response (handler request)
          body-new {:struct (->> (get-in response [:body :hits :hits])
                                 (map pack-search-obj))
                    :query (get-in request [:params :q])
                    :page 1
                    :page_size (get-in request [:params :raw :page_size])}]
      (assoc response :body body-new))))

(defn wrap-search-exact [handler]
  (fn [request]
    (let [response (handler request)
          pack-function pack-search-obj
          body-new (->> (get-in response [:body :hits :hits])
                        (map pack-function)
                        (first))]
      (assoc response :body body-new))))

(defn wrap-count [handler]
  (fn [request]
    (let [response (handler request)
          count (get-in response [:body :count])]
      (assoc response :body {:count count}))))

(defn wrap-random [handler]
  (fn [request]
    (let [response (handler request)]
      (assoc response :body (->> (get-in response [:body :hits :hits])
                                 (first)
                                 (get-obj))))))
