(cl:in-package #:asdf-user)

(defsystem #:org2html-web-app
  :description "Utility to display org mode notes as html over HTTP."
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:parenscript #:cl-json #:cl-who #:jfh-utility #:jfh-web-core)
  :components ((:file package)
               (:file common/configure)
	       (:file common/utility)
               (:file common/page-include)
               (:file common/page-handlers)))

