;;;; web-specific utility
(cl:in-package #:org2html-web-app)

(defun format-for-web (string)
  "Replace line-breaks \n with the HTML <br /> element, and double-up any tildas."
  (reduce
   (lambda (acc cur)
     (cl-ppcre:regex-replace-all (string (car cur)) acc (cdr cur)))
   (list
    (cons #\Return "<br />")
    ;; (cons #\~ "~~")
    )
   :initial-value string))
