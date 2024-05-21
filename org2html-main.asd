;; (cl:in-package #:asdf-user)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun read-complete-file (path)
  "read complete file all at once"
  (with-open-file (in path :if-does-not-exist :create)
    (read in nil)))

(defun load-everything (app-root)
  (flet ((get-library-path (library-path)
           (make-pathname :directory (concatenate 'string app-root library-path))))
    (push (get-library-path "utility") asdf:*central-registry*)
    (asdf:load-system "jfh-utility")
    (push (get-library-path "app-core") asdf:*central-registry*)
    (asdf:load-system "jfh-app-core")
    (push (get-library-path "web-core") asdf:*central-registry*)
    (asdf:load-system "jfh-web-core")
    (push (get-library-path "org2html") asdf:*central-registry*)
    (asdf:load-system "org2html")
    (push (get-library-path "web-app") asdf:*central-registry*)
    (asdf:load-system "org2html-web-app")
    (push (get-library-path "org2html") asdf:*central-registry*)
    ;; (asdf:load-system "org2html-main")
    )
  (print "dependencies loaded"))

(let* ((file (read-complete-file ".config"))
       (app-root (cdr (assoc :app-root file))) 
       ;; (lib-root (cdr (assoc :lib-root file)))
       ;; (jfh-web-lib-path (make-pathname :directory (concatenate 'string lib-root "/web/jfh-web")))
       ;; (jfh-web-source-path (probe-file (concatenate 'string "/" lib-root "/web/jfh-web/jfh-web.lisp")))
       ;; (main-app-path (make-pathname :directory app-root))
       )

  (load-everything app-root)
  ;; (push jfh-web-lib-path asdf:*central-registry*)
  ;; (push main-app-path asdf:*central-registry*)
  ;; (asdf:load-system "jfh-web")
  ;; (compile-file jfh-web-source-path)
  )

(asdf:defsystem #:org2html-main
  :description "Convert (simple!!) org to html and serve it up via HTTP"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file package)
               (:file application)
               (:file main))
  :depends-on (:hunchentoot :cl-who :cl-ppcre :org2html))

(defun buildapp ()
  (asdf:load-system :org2html-main)
  (sb-ext:save-lisp-and-die "org2html-app"
                     :toplevel 'cl-user::jfh-app-main
                     :executable t)
  (print "main loaded"))
