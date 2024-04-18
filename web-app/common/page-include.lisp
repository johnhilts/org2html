;;;; Web page sections to include with other pages
(cl:in-package #:org2html-web-app)

(defun common-header (html-head &optional (use-light-mode t))
  (let ((title (cadr (assoc :title html-head)))
        (code-language (cadr (assoc :code-language html-head)))
        (style-prefix (if (and use-light-mode (member use-light-mode '("t" "true") :test #'string-equal)) "light" "dark")))
    (who:with-html-output-to-string
        (*standard-output* nil :indent t)
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title (who:str (format nil "Org 2 Html Utility - ~A" title)))
       (:link :type "text/css"
              :rel "stylesheet"
              :href (format nil "~A~A~D" (web:static-root *web-configuration*) (format nil "/~A-styles.css?v=" style-prefix) (get-version)))
       (when code-language
         (who:htm
          (:link :type "text/css"
                 :rel "stylesheet"
                 :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/default.min.css")
          (:script :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js")
          (:script :src (who:htm (format nil "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/~A.min.js" code-language)))
          (:script (who:str "hljs.highlightAll();"))))))))
