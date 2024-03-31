(in-package #:cl-user)

(defpackage #:org2html-web-app
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core))
  (:export
   #:*web-configuration*
   #:*static-paths-maps*
   #:setup-dispatch-for-all-html-files))
