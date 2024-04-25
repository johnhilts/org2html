(in-package #:cl-user)

(defpackage #:jfh-utility
  (:use #:common-lisp)
  (:export
   #:fetch-or-create-data
   #:read-complete-file
   #:write-complete-file
   #:generate-unique-token
   #:hash-password
   #:serialize-to-json
   #:string-starts-with
   #:trim-space
   #:empty-string-p))
