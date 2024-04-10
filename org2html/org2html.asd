(cl:in-package #:asdf-user)

(defsystem #:org2html
  :description "Convert (simple!!) org to html and serve it up via HTTP"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file package)
               (:file org2html))
  :depends-on (:hunchentoot :cl-who :cl-ppcre :jfh-utility))
