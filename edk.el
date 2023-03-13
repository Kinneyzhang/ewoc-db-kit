(defun zk-parse-success-notes ()
  (let ((file "c:/Users/26289/Hackings/ewoc-db-kit/past-notes")
        str date-lst content-lst)
    (with-current-buffer (find-file-noselect file)
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    (setq date-lst (mapcar (lambda (title)
                             (save-match-data
                               (string-match "## +\\([-0-9]+\\).*" title)
                               (match-string-no-properties 1 title)))
                           (split-string str "\\(^[^#].+\n*\\)+" t "\n")))
    (setq content-lst (split-string str "^## +[-0-9].+" t "\n"))
    (dotimes (i (length date-lst))
      (zksummary-db-add "daily" (nth i content-lst) (nth i date-lst)))))

;; 513

(zk-parse-success-notes)

(re-search-forward "\\(^[^#]+ .+\n*\\)+" nil t)

;; emacs-lisp
