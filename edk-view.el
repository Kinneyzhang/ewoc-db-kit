(defun edk-tpl-)

(defun edk-view-create (&rest plist)
  (if-let ((model (plist-get plist :model))
           )
      ))

(defun test-tpl1 (plist)
  (if-let ((id (plist-get plist :id))
               (name (plist-get plist :name))
               (content (plist-get plist :content)))
      (insert id " " name " " content)
    (insert "no data")))

(defun test-ewoc ()
  (let ((ewoc (ewoc-create 'test-tpl "header" "footer")))
    (ewoc-enter-last
     ewoc
     '(:id "111" :name "name1" :content "content111111111"))
    (ewoc-enter-last
     ewoc
     '(:id "222" :name "name2" :content "content222222222"))
    (ewoc-enter-last
     ewoc
     '(:id "333" :name "name3" :content "content333333333"))))
