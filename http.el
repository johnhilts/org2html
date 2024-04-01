(defun my/http-post (url data)
  "Send DATA to URL as a POST request."
  (let ((url-request-method "POST")
     	(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (format "org-text=%s" data)))
    (url-retrieve url (lambda (status) (switch-to-buffer (current-buffer))))))

(defun my/example-http-post ()
    (my/http-post "http://192.168.1.18:5096/receive" "* One\r\n** Two"))

(defun my/example-http-post-2 ()
    (my/http-post "http://192.168.1.18:5096/receive" "* One\r\n** Two\r\n- Item 1\r\n - Item 2\r\n   - Item 3"))

(defun my/example-http-post-with-a-table ()
    (my/http-post "http://192.168.1.18:5096/receive" "* One\r\n** Two\r\n- Item 1\r\n - Item 2\r\n   - Item 3\r\n| One | Two |\r\n+-----+-----+\r\n| 333 | 444 |"))

(defun my/example-http-post-with-a-table-no-hl ()
    (my/http-post "http://192.168.1.18:5096/receive" "* One\r\n** Two\r\n- Item 1\r\n - Item 2\r\n   - Item 3\r\n| One | Two |\r\n| 333 | 444 |"))

(defun my/show-html ()
  (url-retrieve "http://192.168.1.18:5096/show.html" (lambda (status) (switch-to-buffer (current-buffer)))))
