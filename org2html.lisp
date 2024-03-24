(cl:in-package #:org2html)

(defparameter *patterns* (list
                          (cons "^(\\s+)?\\-\\s+(\\w+)" :li)
                          (cons "^[\\s+]?\\*\\s+(\\w+)" :h1)
                          (cons "^[\\s+]?\\*\\*\\s+(\\w+)" :h2)
                          (cons "^[\\s+]?\\*\\*\\*\\s+(\\w+)" :h3)
                          (cons "^[\\s+]?\\*\\*\\*\\*\\s+(\\w+)" :h4)))

(defun make-scanner-from-pattern (pattern)
  "make a regex scanner from a pattern"
  (ppcre:create-scanner pattern))

(defun make-scanners ()
  "Pre-make scanners for the patterns we want to use."
  (loop for pattern in *patterns*
        collect
        (cons (make-scanner-from-pattern (car pattern)) (cdr pattern))))

(defclass parsed-line()
  ((%text :reader text
	  :initarg :text)
   (%html-tag :reader html-tag
	      :initarg :html-tag)
   (%match-group-end :reader match-group-end
		     :initarg :match-group-end
                     :initform nil)
   (%nest-level :reader nest-level
                :initarg :nest-level
                :initform nil))
  (:documentation "The results of a parsed line."))

(defmethod print-object ((parsed-line parsed-line) stream)
  "Print parsed line."
  (print-unreadable-object (parsed-line stream :type t)
    (with-accessors ((text text)
                     (html-tag html-tag)
                     (match-group-end match-group-end)
                     (nest-level nest-level))
        parsed-line
      (format stream
	      "Text: ~S, HTML Tag: ~S~:[~:;, Regex Match Group End Pos: ~:*~D~]~:[~:;, Nest Level: ~:*~D~]"
	      text html-tag match-group-end nest-level))))

(defgeneric get-nest-level (parsed-line group-end))
(defmethod get-nest-level ((previous-line parsed-line) group-end)
  "How nested is the list item? Input: previous parsed line and current lines length of beginning spaces. Output: the number of nested levels."
  (if (> group-end 0)
      (if (match-group-end previous-line)
          (let ((previous-group-end (match-group-end previous-line)))
            (cond
              ((> group-end previous-group-end)
               (1+ (nest-level previous-line)))
              ((= group-end previous-group-end)
               (nest-level previous-line))
              ((< group-end previous-group-end)
               (1- (nest-level previous-line)))))
          0)
      0))

(defun parse-org-text (org-text)
  "Given a string of text, parse it. Input: text. Output: list suitable for use with something like cl-who."
  (let ((regex-scanners (make-scanners))
        (string (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
        (parsed-lines ())
        (item-list ()))
    (with-input-from-string (in org-text)
      (with-output-to-string (out string)
        (do ((i 1 (incf i))
             (line #1=(read-line in nil nil) #1#)
             (match-found nil))
            ((null line) (values (nreverse parsed-lines) (nreverse item-list) (format nil "~%Original Lines:~%~A~%~D line~:P read." string (1- i))))
          (loop for scanner in regex-scanners
                do
                   (setq match-found nil)
                   (multiple-value-bind (match-start _match-end group-starts group-ends)
                       (ppcre:scan (car scanner) line)
                     (declare (ignore _match-end))
                     (when match-start
                       (setq match-found t)
                       (let* ((is-list-item (eql (cdr scanner) :li))
                              (group-end (if is-list-item (if (null (aref group-ends 0)) 0 (aref group-ends 0)) 0))
                              (nest-level (if (eql (cdr scanner) :li) (get-nest-level (car item-list) group-end) nil))
                              (group-index (1- (length group-starts)))
                              (text (subseq line (aref group-starts group-index))))
                         (if nest-level
                             (progn
                               (when (null item-list)
                                 ;; (and
                                 ;;      (null item-list)
                                 ;;      (not (nest-level (car item-list))))
                                 (push (list (make-instance 'parsed-line :text "" :html-tag :ul)) item-list)
                                 (push item-list (list parsed-lines)))
                               (push (make-instance 'parsed-line :text text :html-tag (cdr scanner) :match-group-end group-end :nest-level nest-level) item-list))
                             (push (make-instance 'parsed-line :text text :html-tag (cdr scanner)) parsed-lines)))
                       (return))))
          (when (not match-found)
            (push (make-instance 'parsed-line :text line :html-tag :span) parsed-lines))
          (format out "Line # ~D: ~A~%" i line))))))

(defgeneric generate-html (parsed-line))
(defmethod generate-html ((parsed-line parsed-line))
  "Takes list of type parsed-line and converts to html"
  (list
   (html-tag parsed-line)
   (text parsed-line)))
(defmethod generate-html (parsed-lines)
  "Takes list of type parsed-line and converts to html"
  (append
   (list :div)
   (loop for parsed-line in parsed-lines
         collect
         (generate-html parsed-line))))

(defmacro with-my-html-output-to-string ((stream &key prologue indent) &body body)
  `(who:with-html-output-to-string (,stream nil :prologue ,prologue :indent ,indent)
     (:html (:body ,@body))))

(defun who-test ()
  (let ((my-html (generate-html (parse-org-text *test-text*))))
    (eval
     `(with-my-html-output-to-string (*standard-output* :prologue t :indent t)
        ,my-html))))

;; (defmacro with-my-html-output-to-string ((stream &key prologue indent) &body body)
;;   `(who:with-html-output-to-string (,stream nil :prologue ,prologue :indent ,indent)
;;      (:html (:body ,@body))))

(let ((my-html '(:div (:span "more html"))))
  (eval
   `(with-my-html-output-to-string (*standard-output* :prologue t :indent t)
      ,my-html)))

;; this works!!
(let ((my-html (generate-html (parse-org-text *test-text*))))
  (eval
   `(with-my-html-output-to-string (*standard-output* :prologue t :indent t)
      ,my-html)))

;; without eval
;; (setf my-html '(:div (:span "more html")))
(let ((my-html (generate-html (parse-org-text *test-text*))))
  (who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (format nil "~a" '(:html (:body my-html)))))

(defun chat-gpt-generate-html (tags)
  (who:with-html-output-to-string (s nil :prologue t :indent t)
    `(who:htm (:html (:body ,@tags)))))

;; (setf my-html '(:div (:span "more html")))
(let ((my-html (generate-html (parse-org-text *test-text*))))
  (setf my-html '(:div (:span "more html")))
  (generate-html my-html))

;; something else to try - didn't work
(defun my-generate-html (tags)
  (who:with-html-output-to-string (s nil :prologue t :indent t)
    `(html (body ,@tags))))

;;(setf my-html '(:div (:span "more html")))
(let ((my-html '(:div (:span "more html"))))
  (my-generate-html my-html))


#|
(let ((html (who:with-html-output-to-string (*standard-output* nil :indent t) (:div (:a :href "link.html")))))
            (who:with-html-output-to-string (*standard-output* nil :indent t)
(:html (:body (who:str html)))))
|#

;; (let ((scanner (make-scanner-from-pattern *level-1-pattern*)))
;;             (ppcre:scan scanner "* level 1"))

;; (make-instance 'parsed-line :text "text" :html-tag :h1 :match-group-end 2 :nest-level 1)
(defparameter *test-text* "* Top
** Next Level
Just some normal text here.
*** Nexter Level
- Item 1
 - Sub-Item 1
- Item 2
 **** Even nexter level
- Item 3
 - Sub-Item 4
   - Sub-sub-Item 5
- Item 6")

#| sub-lists handling - better way??

(defparameter *jfh/my-list* ())
(push 'ul *jfh/my-list*)
(push (list (list 'item1)) (cdr *jfh/my-list*))
(push (list (list 'item2)) (cdr *jfh/my-list*))
(push (list (list 'sub-item1)) (caadr *jfh/my-list*)) ; put sub-item1 under item2
|#
