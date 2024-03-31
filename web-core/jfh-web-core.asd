(cl:in-package #:asdf-user)

(defsystem #:jfh-web-core
  :description "Core functionality available for a web app."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot)
  :components ((:file package)
               (:file web-core-protocol)
               (:file macros)
               (:file web-core)))
