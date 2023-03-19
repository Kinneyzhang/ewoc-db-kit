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

(defun edk--update-setexp (list)
  (edk-list->vector
   (mapcar (lambda (pair)
             (append '(=) pair))
           list)))
