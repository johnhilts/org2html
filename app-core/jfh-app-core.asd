(cl:in-package #:asdf-user)

(defsystem #:jfh-app-core
  :description "Core functionality available for an app."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-json #:swank #:ironclad)
  :components ((:file package)
               (:file app-core-protocol)
               (:file app-core)))
