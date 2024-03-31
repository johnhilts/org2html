;;;; protocol related to general application concerns.
(cl:in-package #:jfh-app-core)

(defclass application-configuration ()
  ((%swank-port
    :reader swank-port
    :initarg :swank-port)
   (%swank-interface
    :reader swank-interface
    :initarg :swank-interface)
   (%settings-file-path
    :reader settings-file-path
    :initarg :settings-file-path)
   (%user-path-root
    :reader user-path-root
    :initarg :user-path-root
    :initform (error "Value required for :user-path-root")))
  (:documentation "Application configurations."))

(defgeneric start-swank (application-configuration)
  (:documentation "Input: application-configuration. Start swank with the provided configuration settings."))
 
(defgeneric stop-swank (application-configuration)
  (:documentation "Input: application-configuration. Stop swank with the provided configuration settings."))

(defclass application-user ()
  ((%user-id
    :reader user-id
    :initarg :user-id
    :initform "")
   (%user-login
    :reader user-login
    :initarg :user-login))
  (:documentation "Application user info - the very bare minimum."))

(defclass application-secure-user (application-user)
  ((%user-password
    :reader user-password
    :initarg :user-password
    :initform ""))
  (:documentation "Application user secure info."))

(defclass application-meta-user (application-user)
  ((%create-date
    :reader create-date
    :initarg :create-date
    :initform (get-universal-time))
   (%disable
    :reader disable
    :initarg :disable
    :initform nil))
  (:documentation "Application user info with meta-data."))

(defclass user-index-entry ()
  ((%user-login
    :reader user-login
    :initarg :user-login)
   (%user-id
    :reader user-id
    :initarg :user-id))
  (:documentation "User index entry. Link User ID to persisted data."))

(defgeneric find-user-path (application-user application-configuration)
  (:documentation "Input: application-user and app-configuration. Output: user path."))

(defgeneric find-user-index-entry (user-login application-configuration)
  (:documentation "Input: User name (login) and app-configuration. Output: user index entry."))

(defgeneric make-user-index-entry (application-user)
  (:documentation "Input: application-user. Output: user index entry."))

(defgeneric user-index-entry->list (user-index-entry)
  (:documentation "Input: user index entry. Output: regular list. Conversion function."))

(defgeneric save-user (file-name user-info-list application-user application-configuration) ;; TODO think of better function/method names
  (:documentation "Input: file-name, user info list (not a class), application-user and app-configuration. Output: user info list. Persist application user info."))

(defgeneric save-application-user (application-user application-configuration)
  (:documentation "Input: application-user and app-configuration. Output: application-user. Persist application user info."))

(defgeneric save-new-application-user (application-user application-configuration)
  (:documentation "Input: application-user. Output: application-user. Persist *NEW* application user info."))
