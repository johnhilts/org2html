;;;; app core; start, stop swank
(cl:in-package #:jfh-app-core)

(defmethod print-object ((application-configuration application-configuration) stream)
  "Print application configuration."
  (print-unreadable-object (application-configuration stream :type t)
    (with-accessors ((swank-port swank-port)
                     (swank-interface swank-interface)
                     (settings-file-path settings-file-path)
                     (user-path-root user-path-root))
        application-configuration
      (format stream
	      "~:[~:;Swank Port: ~:*~d~]~:[~:;, Swank Interface: ~:*~a~]~:*~:[~:;, ~]Settings File: ~s, User Path: ~s"
	      swank-port swank-interface settings-file-path user-path-root))))

(defmethod initialize-instance :after ((application-configuration application-configuration) &key)
  "Initializations:
- Add ending path separator to any slot with a path for a value"
  (let ((user-path-root #1=(slot-value application-configuration '%user-path-root)))
    (unless (char= #\/ (char user-path-root (1- (length user-path-root))))
      (setf #1# (format nil "~a/" user-path-root)))))

(defun make-application-configuration (&optional (swank-port 4005) (swank-interface "localhost") (settings-file-path "./settings.sexp") (user-path-root "./users/"))
  "Get configuration info from the file system and hydrate application-configuration object.
Input: default configuration values.
Output: application-configuration object."
  (let* ((call-back #'(lambda (settings)
			(if settings
			    (make-instance 'application-configuration
					   :swank-port (getf settings :swank-port)
					   :swank-interface (getf settings :swank-interface)
					   :settings-file-path settings-file-path
                                           :user-path-root (getf settings :user-path-root))
			    (make-instance 'application-configuration
					   :swank-port swank-port
					   :swank-interface swank-interface
					   :settings-file-path settings-file-path
                                           :user-path-root user-path-root)))))
    (jfh-utility:fetch-or-create-data settings-file-path call-back)))

(defmethod start-swank ((application-configuration application-configuration))
  "Input: application-configuration. Start swank on the configured port."
  (with-accessors ((swank-port swank-port) (swank-interface swank-interface)) application-configuration
    (flet ((start-swank-server ()
             (restart-case (swank:create-server :port swank-port
					        :interface swank-interface
					        :dont-close t)
	       (skip-swank-start ()
                 :report "Skip Swank Start."
                 (format t "Swank start skipped.")
                 (throw 'swank-start swank-port)))))
      (let ((*debug-io* (make-broadcast-stream)))
        (catch 'swank-start
          (let ((actual-port (start-swank-server)))
	    (format t "Started swank at port: ~A." actual-port)
            actual-port))))))

(defmethod stop-swank ((application-configuration application-configuration))
  (with-accessors ((swank-port swank-port)) application-configuration
    (when swank-port
      (swank:stop-server swank-port)
      (format t "Stopped swank at port: ~A." swank-port))))
