(defun http-post (url data)
  "Send DATA to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data (format "org-text=%S" data)))
    (url-retrieve url (lambda (status) (switch-to-buffer (current-buffer))))))

(defun example-http-post ()
    (http-post "http://192.168.1.18:5096/show" "* One\r\n** Two"))
