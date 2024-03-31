(cl:in-package #:cl-user)

(defun load-local-org2html ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/org2html/org2html/")
  (push #p"/home/jfh/code/lisp/source/org2html/org2html/" asdf:*central-registry*)
  (asdf:load-system "org2html")
  ;; (in-package #:org2html)
  )
