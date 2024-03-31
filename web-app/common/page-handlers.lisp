;;;; Web pages for org mode notes coverted to HTML
(cl:in-package #:org2html-web-app)

(defun get-version () "1")

(tbnl:define-easy-handler (root :URI "/") ()
  "root route handler"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Org 2 Html"))
     (:body
      (:div
       "Welcome to the org2html utility!")
      (:div
       (:a :href "./show" "Show"))))))

(tbnl:define-easy-handler (show :URI "/show") ()
  "show converted org markdown handler"
  (let* ((org-text (tbnl:post-parameter "org-text"))
         (the-html (org2html:build-tree (org2html:parse-org-text (if (zerop (length org-text)) org2html::*test-text* org-text) #'format-for-web))))
    (eval
     `(who:with-html-output-to-string
          (*standard-output* nil :prologue t :indent t)
        (:html
         (who:str (common-header "Org 2 Html"))
         (:body
          (:div
           ,@the-html)))))))

(tbnl:define-easy-handler (add :URI "/add") ()
  "add org markdown handler"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Org 2 Html - add org markdown"))
     (:body
      (:div
       (:form :method "post" :action "show"
	      (:div
	       (:div (:textarea :id "org-text" :name "org-text" :placeholder "Add org mode markdown text here." :autofocus "autofocus" :rows "40" :cols "150"))
	       (:div (:button "Add")))))))))

(tbnl:define-easy-handler (version-page :uri "/version") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Version"))
     (:body
      (:div "Version")
      (:div (who:str(get-version)))))))

#|
(tbnl:create-regex-dispatcher "*.html" #'handle-static-file)
(tbnl:create-regex-dispatcher ".*\.html" (lambda () (tbnl:handle-static-file (format nil "/home/jfh/code/lisp/source/org2html~A" (tbnl:script-name tbnl:*request*)))))
(push
      (tbnl:create-static-file-dispatcher-and-handler (car mapping) (cdr mapping))
tbnl:*dispatch-table*)

;; this is how to add a catch-all handler for *.html!
(push
 (tbnl:create-regex-dispatcher
  ".*\.html"
  (lambda () (tbnl:handle-static-file (format nil "/home/jfh/code/lisp/source/org2html/~A" (tbnl:script-name tbnl:*request*)))))
 tbnl:*dispatch-table*)
|#
