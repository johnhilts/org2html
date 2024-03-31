;;;; Configure settings for this web app
(cl:in-package #:org2html-web-app)

(defvar *web-configuration*)

(defparameter *static-paths-maps*
  '(("/favicon.ico" "ez-favicon.ico")
    ("/styles.css" "static/styles.css")))

(defun setup-dispatch-for-all-html-files ()
  (push
   (tbnl:create-regex-dispatcher
    ".*\.html"
    (lambda () (tbnl:handle-static-file (format nil "/home/jfh/code/lisp/source/org2html/html/~A" (tbnl:script-name tbnl:*request*)))))
   tbnl:*dispatch-table*))
