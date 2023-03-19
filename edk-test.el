(setq edk-db-file "~/edk/edk.db")

(setq edk-db-models
      '((summary [(id :primary-key)
                  (type :not-null)
                  (content :not-null)
                  (summary_time :not-null)
                  (create_time :not-null)])))

;; '(id type content summary_time create_time)

(edk-db-crud [:select * :from summary])

(edk-model-insert
 'summary
 `(( :content "content111"
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
     :summary_time "2023-03-19"
     :create_time ,(edk-second))))

(edk-model-insert
 'summary
 `( :content "contentaaa"
    :type "daily" :id ,(edk-uuid)
    :summary_time "2023-03-19"
    :create_time ,(edk-second)))

(edk-model-delete 'summary)

(edk-model-update
 'summary '((type "monthly") (content "content234"))
 '(= content "content111"))

(edk-model-count 'summary '(= type "daily"))

(edk-model-query 'summary '*
                 '(= content "content333"))

(edk-db-crud `[:select * :from summary])
(vectorp '(funcall count 1))

(edk-model-query 'summary '(type id)
                 '(not (= type "daily")))

(edk-model-crud [:select * :from summary])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: modify the parameter of api to kvs

(edk-model-count 'summary '(and (= type "monthly")
                                (= content "content234")))
(edk-model-count
 :model summary
 :conds (and (= type "monthly")
             (= content "content234")))


(edk-model-filter 'summary '(and (= type "daily")
                                 (= content "contentaaa")))
(edk-model-filter
 :model summary
 :conds (and (= type "daily")
              (= content "contentaaa")))

(edk-model-get 'summary '(and (= type "daily")
                              (= content "contentaaa")))
(edk-model-get
 :model summary
 :conds (and (= type "daily")
              (= content "contentaaa")))

(edk-model-exclude 'summary '(= type "daily"))
(edk-model-exclude
 :model summary
 :conds (= type "daily"))
