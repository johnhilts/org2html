(cl:in-package #:asdf-user)

(defsystem #:jfh-utility
  :description "General utilities."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-json #:ironclad)
  :components ((:file package)
               (:file strings)
               (:file crypto)
               (:file guid)
               (:file io)
               (:file json)
               (:file util)))
