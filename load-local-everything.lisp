(cl:in-package #:cl-user)

(defun load-local-everything ()
  (load "/home/jfh/code/lisp/source/org2html/utility/load-local-utility.lisp")
  (load-local-utility)
  (print "utilty loaded")

  (load "/home/jfh/code/lisp/source/org2html/app-core/load-local-app-core.lisp")
  (load-local-app-core)
  (print "app-core loaded")

  (load "/home/jfh/code/lisp/source/org2html/web-core/load-local-web-core.lisp")
  (load-local-web-core)
  (print "web-core loaded")

  (load "/home/jfh/code/lisp/source/org2html/org2html/load-local-org2html.lisp")
  (load-local-org2html)
  (print "org2html loaded")

  (load "/home/jfh/code/lisp/source/org2html/web-app/load-local-web-app.lisp")
  (load-local-web-app)
  (print "web-app loaded")

  (swank:set-default-directory "/home/jfh/code/lisp/source/org2html/")
  (push #p"/home/jfh/code/lisp/source/org2html/" asdf:*central-registry*)
  (asdf:load-system "org2html-main")
  (print "main loaded")
  ;; (in-package #:org2html)
  ;; (load "internal.lisp")
  )
