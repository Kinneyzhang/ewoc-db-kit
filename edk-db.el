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

;;; Model API

(defun edk-model-count (&rest plist)
  (if-let ((model (plist-get plist :model)))
      (if-let ((conds (plist-get plist :conds)))
          (caar (edk-db-crud `[:select (funcall count 1) :from ,model :where ,conds]))
        (caar (edk-db-crud `[:select (funcall count 1) :from ,model])))
    (edk-error-keywords-missing :model)))

(defun edk-model-insert (&rest plist)
  (if-let* ((model (plist-get plist :model))
            (values (plist-get plist :values))
            (data (edk-model-values model values)))
      (edk-db-crud `[:insert :into ,model :values ,data])
    (edk-error-keywords-missing :model :values)))

(defun edk-model-delete (&rest plist)
  (if-let ((model (plist-get plist :model))
           (count 0))
      (progn
        (if-let* ((conds (plist-get plist :conds)))
            (progn
              (setq count (edk-model-count :model model :conds conds))
              (edk-db-crud `[:delete :from ,model :where ,conds]))
          (setq count (edk-model-count :model model))
          (edk-db-crud `[:delete :from ,model]))
        count)
    (edk-error-keywords-missing :model)))

(defun edk-model-update (&rest plist)
  (if-let* ((model (plist-get plist :model))
            (values (plist-get plist :values))
            (values-exp (edk--format-setexp values))
            (count 0))
      (progn
        (if-let ((conds (plist-get plist :conds)))
            (progn
              (setq count (edk-model-count :model model :conds conds))
              (edk-db-crud `[:update ,model :set ,values-exp :where ,conds]))
          (setq count (edk-model-count :model model))
          (edk-db-crud `[:update ,model :set ,values-exp]))
        count)
    (edk-error-keywords-missing :model :values)))

(defun edk-model-query (&rest plist)
  (if-let ((model (plist-get plist :model))
           (fields '*))
      (progn (setq fields (or (edk--format-fields
                               (plist-get plist :fields))
                              fields))
             (if-let ((conds (plist-get plist :conds)))
                 (edk-db-crud `[:select ,fields :from ,model :where ,conds])
               (edk-db-crud `[:select ,fields :from ,model])))
    (edk-error-keywords-missing :model)))

(defun edk-model-crud (&rest plist)
  (if-let ((sql (plist-get plist :sql)))
      (if-let ((args (plist-get plist :args)))
          (edk-db-crud sql args)
        (edk-db-crud sql))
    (edk-error-keywords-missing :sql)))

(defun edk-model-all (&rest plist)
  (if-let ((model (plist-get plist :model)))
      (edk-model-query :model model :fields '*)
    (edk-error-keywords-missing :model)))

(defun edk-model-filter (&rest plist)
  (if-let ((model (plist-get plist :model))
           (conds (plist-get plist :conds)))
      (edk-model-query :model model :fields '* :conds conds)
    (edk-error-keywords-missing :model :conds)))

(defun edk-model-get (&rest plist)
  (if-let ((model (plist-get plist :model))
           (conds (plist-get plist :conds)))
      (let ((data (edk-model-query :model model :fields '*
                                   :conds conds)))
        (when (= (length data) 1)
          (car data)))
    (edk-error-keywords-missing :model :conds)))

(defun edk-model-exclude (&rest plist)
  (if-let ((model (plist-get plist :model))
           (conds (plist-get plist :conds)))
      (edk-model-query :model model :fields '*
                       :conds (append '(not) (list conds)))
    (edk-error-keywords-missing :model :conds)))

;;; 基于外键的高级查询等
;; ........


(provide 'edk-db)
