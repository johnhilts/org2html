(defun http-post (url data)
  "Send DATA to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data "query=join&title15=emacs-tutorial+%28Keith+Waclena%29&title16=hacking-your-way-emacs+%28Marcin+Borkowski%29&title17=Linux+in+a+Nutshell+%28Siever%2C+Ellen%3BFiggins%2C+Stephen%3BLove%2C+Robert%3BRobbins%2C+Arnold%29"))
    (url-retrieve url (lambda (status) (switch-to-buffer (current-buffer))))))

(defun example-http-port ()
    (http-post "https://192.168.1.18:5095/search" "Join"))
