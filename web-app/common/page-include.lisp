;;;; Web page sections to include with other pages
(cl:in-package #:org2html-web-app)

(defun common-header (title)
  (who:with-html-output-to-string
      (*standard-output* nil :indent t)
    (:head
     (:meta :charset "utf-8")
     (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
     (:title (who:str (format nil "Org 2 Html Utility - ~A" title)))
     (:link :type "text/css"
            :rel "stylesheet"
            :href (format nil "~A~A~D" (web:static-root *web-configuration*) "/styles.css?v=" (get-version))))))
