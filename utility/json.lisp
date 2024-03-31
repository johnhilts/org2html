;;;; JSON (de-)serialization
(cl:in-package #:jfh-utility)

(defun serialize-to-json (lisp-object)
  "Input: lisp Output: JSON object."
  (cl-json:encode-json-to-string lisp-object))
