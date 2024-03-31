;;;; Web core macros
(cl:in-package #:jfh-web-core)

(defmacro define-api-endpoint (name end-point params &body body)
  `(tbnl:define-easy-handler (,name :uri ,end-point) (,@params)
     "macro to DRY REST endpoint declarations"
     (setf (tbnl:content-type*) "application/json")
     (let* ((raw-data  (tbnl:raw-post-data :force-text t))
            (verb (tbnl:request-method tbnl:*request*)))
       ,@body)))
