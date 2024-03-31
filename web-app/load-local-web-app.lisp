(cl:in-package #:cl-user)

(defun load-local-web-app ()
  (swank:set-default-directory "/home/jfh/code/lisp/source/org2html/web-app/")
  (push #p"/home/jfh/code/lisp/source/org2html/web-app/" asdf:*central-registry*)
  (asdf:load-system "org2html-web-app")
  ;; (in-package #:org2html-web-app)
  )
