(in-package #:jfh-utility)

(define-condition file-missing-error (file-error)
  ((app-message :initarg :app-message
		:reader app-message))
  (:report (lambda (condition stream)
             (format stream "~AReported file name: ~S." (app-message condition) (file-error-pathname condition)))))

(defun read-complete-file (path)
  "read complete file all at once"
  (with-open-file (in path :if-does-not-exist :create)
    (read in nil)))

(defun write-complete-file (path list)
  "write complete file all at once"
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (prin1 list out))) ;; print is just like prin1, except it precedes each output with a line break, and ends with a space
