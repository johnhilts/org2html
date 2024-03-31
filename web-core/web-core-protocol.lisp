;;;; protocol related to web core
(cl:in-package #:jfh-web-core)

(defclass web-configuration ()
  ((%http-port
    :reader http-port
    :initarg :http-port)
   (%ssl-port
    :reader ssl-port
    :initarg :ssl-port)
   (%static-root
    :reader static-root
    :initarg :static-root)
   (%application-configuration
    :reader jfh-app-core:application-configuration
    :initarg :application-configuration))
  (:documentation "Application configurations."))

(defclass web-application ()
  ((%hunchentoot-acceptor
    :reader hunchentoot-acceptor
    :initarg :hunchentoot-acceptor)
   (%web-configuration
    :reader web-configuration
    :initarg :web-configuration))
  (:documentation "Web application."))

(defgeneric start-hunchentoot (web-configuration)
  (:documentation "Input: application-configuration. Start hunchentoot web-server with the provided configuration settings."))

(defgeneric start-web-app (web-configuration)
  (:documentation "Input: application-configuration object and path maps for static assets. Output: web-application object. This will start the web application running on top of hunchentoot, and optionally start swank."))
;; (documentation 'start-web-app 'function)

(defgeneric stop-hunchentoot (web-application)
  (:documentation "Input: web-application. Stop hunchentoot web-server via the provided web-application object."))

(defgeneric stop-web-app (web-application)
  (:documentation "Input: web-application objects. Output: #:web-app-stopped. This will stop the web application. The HTTP port will be released."))
;; (documentation 'stop-web-app 'function)

(defgeneric make-web-application (tbnl:easy-acceptor web-configuration)
  (:documentation "Input: hunchentoot easy-acceptor, application-configuration (default settings) object. Output web-application object."))

