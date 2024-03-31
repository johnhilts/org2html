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

(tbnl:define-easy-handler (root :URI "/show") ()
  "root route handler"
    (let ((my-html (org2html:build-tree (org2html:parse-org-text org2html::*test-text*))))
      (eval
       `(who:with-html-output-to-string
            (*standard-output* nil :prologue t :indent t)
          (:html
           (who:str (common-header "Org 2 Html"))
           (:body
            (:div
             ,@my-html)))))))

(tbnl:define-easy-handler (version-page :uri "/version") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header "Version"))
     (:body
      (:div "Version")
      (:div (who:str(get-version)))))))
