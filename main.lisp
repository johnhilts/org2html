(in-package #:cl-user)

(defun jfh-app-main ()
  (org2html-main::application-start)
  (sb-impl::toplevel-repl nil))
