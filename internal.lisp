(in-package #:cl-user)

(defpackage #:jfh-internal
  (:use #:common-lisp)
  (:local-nicknames (#:web #:jfh-web-core) (#:web-app #:org2html-web-app) (#:main #:org2html-main))
  (:export
   #:%list-all-external-symbols
   #:%current-swank-port
   #:%web-app-running-p
   #:%clear-static-routes))

(in-package #:jfh-internal)

(defun %current-swank-port ()
  "Get the current swank port used by the web app."
  (jfh-app-core:swank-port (jfh-app-core:application-configuration web-app:*web-configuration*)))

(defun %list-all-external-symbols-to-string (&optional (package-list (list 'org2html 'org2html-web-app 'jfh-web-core 'jfh-app-core 'jfh-utility)))
  "List external symbols for given packages. Input: list of packages. Output: string."
  (with-output-to-string (string)
    (let ((list))
      (loop for package-name in package-list
	    do
	       (format string "************************~%~A~%************************~%" package-name)
	       (do-external-symbols (symbol (find-package package-name) list)
		 (push symbol list))
	       (format string "~{~A~%~}" (sort (copy-list list) #'string<))))
    string))

(defun %list-all-external-symbols (&optional (package-list (list 'org2html-main 'org2html 'org2html-web-app 'jfh-web-core 'jfh-app-core 'jfh-utility)))
  "List external symbols for given packages. Input: list of packages. Output: tree."
  (let ((list (list)))
    (loop for package-name in package-list
	  for packages = () then ()
	  for symbols = () then ()
	  for functions = () then ()
	  for macros = () then ()
	  for classes = () then ()
	  for generic-functions = () then ()
	  do
	     (do-external-symbols (symbol (find-package package-name) symbols)
	       (cond ((macro-function symbol)
		      (push (symbol-name symbol) macros))
		     ((fboundp symbol)
		      (if (eql 'standard-generic-function (type-of (symbol-function symbol)))
			  (push (list (symbol-name symbol) (swank-backend:arglist symbol)) generic-functions)
			  (push (list (symbol-name symbol) (swank-backend:arglist symbol)) functions)))
		     ((find-class symbol nil)
		      (push (symbol-name symbol) classes))
		     (t
		      (push (symbol-name symbol) symbols))))
	     (push
	      (nconc packages
		     (list package-name
			   (nconc
			    (if generic-functions (list :generic-functions (sort generic-functions 'string< :key 'car)) nil)
			    (if classes (list :classes (sort classes 'string<)) nil)
			    (if functions (list :functions (sort functions 'string< :key 'car)) nil)
			    (if macros (list :macros (sort macros 'string<)) nil)
			    (if symbols (list :symbols (sort symbols 'string<)) nil))))
	      list))
    list))

(defun %web-app-running-p ()
  "Check if the web app - actually hunchentoot server - is running.
This is meant to be a convenience function;
NOTE use of non-exported symbol."
  (handler-case 
      (null (tbnl::acceptor-shutdown-p (web::hunchentoot-acceptor web::*web-application*)))
    (unbound-variable (condition)
      (let ((name (symbol-name (cell-error-name condition))))
        (if (string-equal (symbol-name 'web::*web-application*) name)
            (format nil "Web App not started; ~(~a~) is undefined." name)
            (format nil "Unbound variable! ~S." condition))))))

(defun %clear-static-routes ()
  "NOTE - Just a convenience method! For Web."
  (setq tbnl:*dispatch-table* (last tbnl:*dispatch-table*)))


(defun %refresh-web-auth-functions ()
  "The function pointers don't automatically updated when a function is re-compiled, so use this to update them."
  (auth:use-web-auth (main::register-web-auth-functions)))

(defun %list-all-package-symbols (package-name)
  "Not sure if I want this."
  (with-output-to-string (s)
    (with-package-iterator (next-symbol (list-all-packages)
					:internal)
      (loop
	(multiple-value-bind (more? symbol) (next-symbol)
	  (if more? 
	      (when (string-equal (package-name (symbol-package symbol)) "ORG2HTML") 
		(format s "Symbol: ~A, Package: ~A~%" symbol (package-name (symbol-package symbol))))
	      (return)))))))


(defun %print-formatted-date (&optional (universal-time (get-universal-time)) (stream (make-string-output-stream)))
  "Input: Integer representing time; output stream. Output: Formatted date. Example: \"Sunday, 3/17/2024 14:20:0 UTC -8 (ds: true)\""
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time universal-time)
    (format stream
	    "~D, ~D/~D/~D ~D:~D:~D UTC ~D (ds: ~A)"
	    (case day (0 "Monday") (6 "Sunday"))
	    month
	    date
	    year
	    hour
	    minute
	    second
	    (* -1 zone)
	    (if daylight-p "true" "false"))
    (get-output-stream-string stream)))
