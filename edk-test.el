(setq edk-db-file "~/edk/edk.db")

(setq edk-db-models
      '((summary [(id :primary-key)
                  (type :not-null)
                  (content :not-null)
                  (summary_time :not-null)
                  (create_time :not-null)])))

;; '(id type content summary_time create_time)
(edk-model-all :model 'summary)
(edk-model-delete :model 'summary)
(edk-model-filter :model 'summary
                  :conds '(= content "content111"))
(edk-model-insert
 :model 'summary
 :values `( :content "content111"
            :type "daily" :id ,(edk-uuid)
            :summary_time "2023-03-19"
            :create_time ,(edk-second)))

(edk-model-insert
 :model 'summary
 :values `(( :content "content111"
             :type "daily" :id ,(edk-uuid)
             :summary_time "2023-03-19"
             :create_time ,(edk-second))
           ( :id ,(edk-uuid)
             :content "content222"
             :type "daily"
             :summary_time "2023-03-19"
             :create_time ,(edk-second))
           ( :content "content333"
             :type "daily" :id ,(edk-uuid)
             :summary_time "2023-03-19"
             :create_time ,(edk-second))
           ( :content "content444"
             :type "daily" :id ,(edk-uuid)
             :summary_time "2023-03-20"
             :create_time ,(edk-second))
           ( :content "content555"
             :type "daily" :id ,(edk-uuid)
             :summary_time "2023-03-20"
             :create_time ,(edk-second))
           ( :content "content666"
             :type "daily" :id ,(edk-uuid)
             :summary_time "2023-03-20"
             :create_time ,(edk-second))))

(edk-model-update
 'summary '((type "monthly") (content "content234"))
 '(= content "content111"))

(edk-model-all :model 'summary)
(edk-model-update
 :model 'summary
 :values '(:type "monthly" :content "content222")
 ;; :conds '(= type "daily")
 )

(edk-model-query 'summary '*
                 '(= content "content333"))

(edk-db-crud `[:select * :from summary])
(vectorp '(funcall count 1))

(edk-model-query 'summary '(type id)
                 '(not (= type "daily")))

(edk-model-crud [:select * :from summary])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: modify the parameter of api to kvs

(edk-model-all
 :model 'summary
 :order-by nil)

(edk-model-count
 :model 'summary
 :conds '(= type "daily"))

(edk-model-filter
 :model 'summary
 :conds '(and (= type "daily")
              (= content "contentaaa"))
 :order-by nil)

(edk-model-get
 :model 'summary
 :conds '(and (= type "daily")
              (= content "contentaaa")))

(edk-model-exclude
 :model 'summary
 :conds '(= type "daily")
 :order-by nil)
