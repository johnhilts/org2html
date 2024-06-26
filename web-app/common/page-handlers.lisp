;;;; Web pages for org mode notes coverted to HTML
(cl:in-package #:org2html-web-app)

(defun get-version () "1")

(tbnl:define-easy-handler (root :URI "/") ()
  "root route handler"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header (list (list :title "Org 2 Html"))))
     (:body
      (:div
       "Welcome to the org2html utility!")
      (:div
       (:a :href "./show" "Show"))))))

(tbnl:define-easy-handler (show :URI "/show") ()
  "show converted org markdown handler"
  (let* ((org-text (tbnl:post-parameter "org-text"))
         (all-html (org2html:build-tree (org2html:parse-org-text (if (zerop (length org-text)) org2html::*test-text* org-text) #'format-for-web)))
         (html-head (cdr (assoc 'org2html:html-head all-html)))
         (html-body (cdr (assoc 'org2html:html-body all-html))))
    (eval
     `(who:with-html-output-to-string
          (*standard-output* nil :prologue t :indent t)
        (:html
         (who:str ,(common-header html-head))
         (:body
          (:div
           ,@html-body)))))))

(tbnl:define-easy-handler (add :URI "/add") ()
  "add org markdown handler"
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header (list (list :title "Org 2 Html - add org markdown"))))
     (:body
      (:div
       (:form :method "post" :action "show"
	      (:div
	       (:div (:textarea :id "org-text" :name "org-text" :placeholder "Add org mode markdown text here." :autofocus "autofocus" :rows "40" :cols "150"))
	       (:div (:button "Add")))))))))

(defun use-jira (org-text)
  "Orchestrates: getting Jira markdown from org mode text, then outputting the Jira markdown embedded in HTML. It's meant to be copy/pasted into a Jira description."
  (let* ((jira-markdown (org2html:build-tree-for-jira (if (zerop (length org-text)) org2html::*test-text* org-text))))
    (who:with-html-output-to-string
        (*standard-output* nil :prologue t :indent t)
      (:html (:body (:div (:text-area (who:str jira-markdown))))))))

(defun use-html (org-text use-light-mode)
  "Orchestrates: getting cl-who tree from org mode text, then saving it as an HTML file that can be served from the web app. It's meant to be viewed in a browser."
  (let* ((all-html (org2html:build-tree (org2html:parse-org-text (if (zerop (length org-text)) org2html::*test-text* org-text) #'format-for-web)))
         (html-head (cdr (assoc 'org2html:html-head all-html)))
         (html-body (cdr (assoc 'org2html:html-body all-html))))
    (org2html:save-as-html (eval
                            `(who:with-html-output-to-string
                                 (*standard-output* nil :prologue t :indent t)
                               (:html
                                (who:str ,(common-header html-head use-light-mode))
                                (:body
                                 (:div
                                  ,@html-body))))))
    (who:with-html-output-to-string
        (*standard-output* nil :prologue t :indent t)
      (:html (:body (:div (:a :href #1="http://192.168.1.18:5096/show.html" #1#)))))))

(defun save-received-text-and-render (org-text use-light-mode use-jira)
  "orchestrates either 1. org -> Jira markdown conversion, or 2. org -> html conversion then saves as a file and returns a url to that file."
  (if (string= "t" use-jira)
      (use-jira org-text)
      (use-html org-text use-light-mode)))

(tbnl:define-easy-handler (receive :uri "/receive") (use-light-mode use-jira)
  "http post handler that orchestrates org -> html conversion then saves as a file and returns a url to that file."
  (let ((org-text (tbnl:post-parameter "org-text")))
    (save-received-text-and-render org-text use-light-mode use-jira)))

(tbnl:define-easy-handler (version-page :uri "/version") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (who:str (common-header (list (list :title "Version"))))
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
