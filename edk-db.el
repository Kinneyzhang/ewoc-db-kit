(require 'emacsql)
(require 'emacsql-sqlite)

(defvar edk-db-file nil)
(defvar edk-db-tables
  '((summary
     [(id :primary-key)
      (type :not-null)
      (content :not-null)
      (summary_time :not-null)
      (create_time :not-null)])))

(defun edk-db--get-conn ()
  "Return the edk database connection with key PATH."
  (gethash edk-db-file edk-db--conn))

(defun edk-db--init (db)
  "Initialize database DB with `edk-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) edk-db-tables)
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

(defun edk-db-query (sql &rest args)
  "Return SQL query on edk database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (edk-db) (apply #'format sql args))
    (apply #'emacsql (edk-db) sql args)))

(defun edk-db-clear ()
  "Clear all data in edk database."
  (interactive)
  (when (file-exists-p edk-db-file)
    (dolist (table (mapcar #'car edk-db--table-schemata))
      (edk-db-query `[:delete :from ,table]))))

(defun edk-db-drop ()
  "Drop the whole edk database."
  (interactive)
  (edk-db--close)
  (delete-file edk-db-file))

(provide 'edk-db)
