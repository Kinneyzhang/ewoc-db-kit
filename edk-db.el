(require 'emacsql)
(require 'emacsql-sqlite)

(defvar edk-db-file nil)

(defvar edk-db-models nil)

(defvar edk-db--conn (make-hash-table :test #'equal)
  "Database connection to edk-db.")

(defun edk-db--get-conn ()
  "Return the edk database connection with key PATH."
  (gethash edk-db-file edk-db--conn))

(defun edk-db--init (db)
  "Initialize database DB with `edk-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) edk-db-models)
      (emacsql db `[:create-table ,table ,schema]))))

(defun edk-db ()
  "Entrypoint to edk sqlite database."
  (unless (and (edk-db--get-conn)
               (emacsql-live-p (edk-db--get-conn)))
    (let ((init-db (not (file-exists-p edk-db-file))))
      (make-directory (file-name-directory edk-db-file) t)
      (let ((conn (emacsql-sqlite edk-db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash edk-db-file conn edk-db--conn)
        (when init-db
          (edk-db--init conn)))))
  (edk-db--get-conn))

(defun edk-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for current edk db."
  (unless db
    (setq db (edk-db--get-conn)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun edk-db-clear ()
  "Clear all data in edk database."
  (interactive)
  (when (file-exists-p edk-db-file)
    (dolist (table (mapcar #'car edk-db--table-schemata))
      (edk-db-crud `[:delete :from ,table]))))

(defun edk-db-drop ()
  "Drop the whole edk database."
  (interactive)
  (edk-db--close)
  (delete-file edk-db-file))

(defun edk-db-crud (sql &rest args)
  "Return SQL query on edk database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (edk-db) (apply #'format sql args))
    (apply #'emacsql (edk-db) sql args)))

;;; Outside API

(defun edk-model-fields (model)
  "Result example: \"'(id type content summary_time create_time)\"."
  (when edk-db-models
    (mapcar #'car (nth 1 (assoc model edk-db-models)))))

(defun edk-model-values (model data)
  "Convert a list of kvs to a vector of values ordered by model fields.

Result examples:

\"[id-val type-val content-val summary_time-val create_time-val]\"

\"([id-val1 type-val1 content-val1 summary_time-val1 create_time-val1]
 [id-val2 type-val2 content-val2 summary_time-val2 create_time-val2]
 ...)\""
  (let ((fields (edk-model-fields model))
        (head (car data)))
    (cond
     ((keywordp head) (edk-entry-field-values data fields))
     ((consp head) (edk-entries-field-vals data fields)))))

(defun edk-model--count (model &optional cond)
  (if cond
      (caar (edk-db-crud `[:select (funcall count 1) :from ,model :where ,cond]))
    (caar (edk-db-crud `[:select (funcall count 1) :from ,model]))))

(defun edk-model-insert (model data)
  (let ((data (edk-model-values model data)))
    (edk-db-crud `[:insert :into ,model :values ,data])))

(defun edk-model-delete (model &optional cond)
  (let ((count (edk-model--count model cond)))
    (if cond
        (edk-db-crud `[:delete :from ,model :where ,cond])
      (edk-db-crud `[:delete :from ,model]))
    count))

(defun edk-model-update (model list &optional cond)
  (let ((exp (edk--update-setexp list))
        (count (edk-model--count model cond)))
    (if cond
        (edk-db-crud `[:update ,model :set ,exp :where ,cond])
      (edk-db-crud `[:update ,model :set ,exp]))
    count))

(defun edk-model-query (model fields &optional cond)
  (let ((fields (if (consp fields)
                    (edk-list->vector fields)
                  fields)))
    (if cond
        (edk-db-crud `[:select ,fields :from ,model :where ,cond])
      (edk-db-crud `[:select ,fields :from ,model]))))

(defun edk-model-crud (sql &rest args)
  (edk-db-crud sql args))

;;; model api

(defun edk-model-count (&rest plist)
  (when-let ((model (plist-get plist :model)))
    (edk-model--count model (plist-get plist :conds))))

(defun edk-model-all (&rest plist)
  (when-let ((model (plist-get plist :model)))
    (edk-model-query model '*)))

(defun edk-model-filter (&rest plist)
  (when-let ((model (plist-get plist :model)))
    (edk-model-query model '* (plist-get plist :conds))))

(defun edk-model-get (&rest plist)
  (when-let ((model (plist-get plist :model)))
    (let* ((conds (plist-get plist :conds))
           (data (edk-model-query model '* conds)))
      (when (= (length data) 1)
        (car data)))))

(defun edk-model-exclude (&rest plist)
  (when-let ((model (plist-get plist :model))
             (conds (plist-get plist :conds)))
    (edk-model-query model '* (append '(not) (list conds)))))

;;; 基于外键的高级查询等
;; ........



(provide 'edk-db)
