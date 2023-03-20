(require 'org-id)

(defun edk-second ()
  (time-convert (current-time) 'integer))

(defun edk-uuid ()
  (org-id-uuid))

(defun edk-k->symbol (keyword)
  "A keyword is a symbol leading with a :.
Converting to a symbol means dropping the :."
  (if (keywordp keyword)
      (intern (substring (symbol-name keyword) 1))
    keyword))

(defun edk-s->keyword (str-or-symbol)
  "Convert STR-OR-SYMBOL into a keyword symbol."
  (let ((str (cond
              ((symbolp str-or-symbol) (symbol-name str-or-symbol))
              ((stringp str-or-symbol) str-or-symbol))))
    (intern (if (eq (aref str 0) ?:) str (concat ":" str)))))

(defun edk-list->vector (list)
  "Convert a LIST to a vector."
  (apply 'vector list))

(defun edk-vector->list (vector)
  "Convert a VECTOR to a list."
  (append vector nil))

(defun edk-entry-field-values (plist fields)
  (edk-list->vector
   (mapcar (lambda (field)
            (plist-get plist (edk-s->keyword field)))
           fields)))

(defun edk-entries-field-vals (plists fields)
  (mapcar (lambda (plist)
            (edk-entry-field-values plist fields))
          plists))

(defun plist->alist (plist)
  "Convert a plist to a alist."
  (if (null plist)
      '()
    (cons
     (cons (car plist) (cadr plist))
     (plist->alist (cddr plist)))))

(defun edk--format-setexp (plist)
  ;; '(:type "monthly" :content "content234")
  ;; [(= type "monthly") (= content "content234")]
  (let ((alist (plist->alist plist)))
    (edk-list->vector
     (mapcar (lambda (cons)
               (list '= (edk-k->symbol (car cons))
                     (cdr cons)))
             alist))))

(defun edk--format-fields (fields)
  (if (consp fields) (edk-list->vector fields) fields))

(defun edk-error-keywords-missing (&rest keywords)
  (let* ((keys-lst (mapcar #'symbol-name keywords))
         (join-str (string-join keys-lst "/")))
    (error "Missing keyword(s) %s when calling model functions." join-str)))
