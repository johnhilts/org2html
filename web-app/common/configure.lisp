;;;; Configure settings for this web app
(cl:in-package #:org2html-web-app)

(defvar *web-configuration*)

(defparameter *static-paths-maps*
  '(("/favicon.ico" "ez-favicon.ico")
    ("/styles.css" "static/styles.css")))
