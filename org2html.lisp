(cl:in-package #:org2html)

(defparameter *patterns-old* (list
                              (cons "^(\\s+)?\\-\\s+(\\w+)" :li)
                              (cons "^[\\s+]?\\*\\s+(\\w+)" :h1)
                              (cons "^[\\s+]?\\*\\*\\s+(\\w+)" :h2)
                              (cons "^[\\s+]?\\*\\*\\*\\s+(\\w+)" :h3)
                              (cons "^[\\s+]?\\*\\*\\*\\*\\s+(\\w+)" :h4)))

(defparameter *elements* (list
                          (make-instance 'element :pattern "^(\\s+)?\\-\\s+(\\w+)" :html-tag :li  :is-item t)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\s+(\\w+)" :html-tag :h1)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\s+(\\w+)" :html-tag :h2)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\*\\s+(\\w+)" :html-tag :h3)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\*\\*\\s+(\\w+)" :html-tag :h4)))

(defclass element ()
  ((%pattern :reader pattern
             :initarg :pattern)
   (%html-tag :reader html-tag
              :initarg :html-tag)
   (%is-item :reader is-item
             :initarg :is-item
             :initform nil)))

(defun make-scanner-from-pattern (pattern)
  "make a regex scanner from a pattern"
  (ppcre:create-scanner pattern))

(defun make-scanners ()
  "Pre-make scanners for the patterns we want to use."
  (loop for element in *elements*
        collect
        (cons (make-scanner-from-pattern (pattern element)) element)))

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
                :initform 0)
   (%sub-list :accessor sub-list
              :initarg :sub-list
              :initform nil))
  (:documentation "The results of a parsed line."))

(defmethod print-object ((parsed-line parsed-line) stream)
  "Print parsed line."
  (print-unreadable-object (parsed-line stream :type t)
    (with-accessors ((text text)
                     (html-tag html-tag)
                     (nest-level nest-level)
                     (sub-list sub-list))
        parsed-line
      (format stream
	      "Text: ~S, HTML Tag: ~S~:[~:;, Nest Level: ~:*~D~]~:[~:;, Sub-List?: ~:*~A~]"
	      text html-tag nest-level (when sub-list t)))))

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
          1)
      1))

(defun parse-org-text (org-text)
  "Given a string of text, parse it. Input: text. Output: list suitable for use with something like cl-who."
  (let ((regex-scanners (make-scanners))
        (string (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
        (parsed-lines ()))
    (with-input-from-string (in org-text)
      (with-output-to-string (out string)
        (do ((i 1 (incf i))
             (line #1=(read-line in nil nil) #1#)
             (match-found nil))
            ((null line) (values (nreverse parsed-lines) (format nil "~%Original Lines:~%~A~%~D line~:P read." string (1- i))))
          (loop for regex-scanner in regex-scanners
                for scanner = (car regex-scanner)
                for element = (cdr regex-scanner)
                for previous-parsed-line = (car parsed-lines)
                do
                   (setq match-found nil)
                   (multiple-value-bind (match-start _match-end group-starts group-ends)
                       (ppcre:scan scanner line)
                     (declare (ignore _match-end))
                     (when match-start
                       (setq match-found t)
                       (let* ((is-list-item (eql (html-tag element) :li))
                              (group-end (if is-list-item (if (null (aref group-ends 0)) 0 (aref group-ends 0)) 0))
                              (nest-level (if (eql (html-tag element) :li) (get-nest-level previous-parsed-line group-end) nil))
                              (group-index (1- (length group-starts)))
                              (text (subseq line (aref group-starts group-index))))
                         (if nest-level
                             (progn
                               (when (or
                                      (zerop (nest-level previous-parsed-line))
                                      (< (nest-level previous-parsed-line) nest-level))
                                 (push (make-instance 'parsed-line :text "" :html-tag :ul :nest-level (nest-level previous-parsed-line)) parsed-lines))
                               (push (make-instance 'parsed-line :text text :html-tag (html-tag element) :match-group-end group-end :nest-level nest-level) parsed-lines))
                             (push (make-instance 'parsed-line :text text :html-tag (html-tag element)) parsed-lines)))
                       (return))))
          (when (not match-found)
            (push (make-instance 'parsed-line :text line :html-tag :span) parsed-lines))
          (format out "Line # ~D: ~A~%" i line))))))

(defun build-tree (parsed-lines &optional (verbose nil))
  (let* ((tree)
         (parents tree)
         (previous-level 0))
    (loop for parsed-line in parsed-lines
          for tag = (html-tag parsed-line)
          for text = (text parsed-line)
          for level = (nest-level parsed-line)
          do
             (cond
               ((> level previous-level)
                (push tree parents)
                (setf tree (car tree)))
               ((< level previous-level)
                (loop repeat (- previous-level level)
                      do
                         (setf (car (car parents)) (reverse tree))
                         (setf tree (pop parents)))))
             (push (if (eql :ul tag) (list :ul) (list tag text)) tree)
             (setf previous-level level)
             (when verbose
               (format t "~&***************Input: ~A~%Tree: ~A~%Parents: ~A~%" parsed-line tree parents))
          finally (when (> level 0)
                    (loop repeat level
                          do
                             (setf (car (car parents)) (reverse tree))
                             (setf tree (pop parents)))))
    (reverse tree)))

(defun build-tree-old3 (parsed-lines)
  "Build a tree based on the parsed lines. Input: list of parsed-line. Output: tree of html tags suitable for feeding to cl-who."
  (let ((index 0)
        (previous-parsed-line))
    (labels ((adjust (parsed-line)
               (incf index)
               (setf previous-parsed-line parsed-line))
             (build-tree-r ()
               (let ((tree))
                 (loop for parsed-line = (nth index parsed-lines)
                       while (< index (length parsed-lines))
                       do
                          (progn
                            ;; (break)
                            (cond
                              ((or
                                (null tree)
                                (= (nest-level parsed-line) (nest-level previous-parsed-line)))
                               (push (list (html-tag parsed-line) (text parsed-line)) tree)
                               (adjust parsed-line))
                              ((> (nest-level parsed-line) (nest-level previous-parsed-line))
                               (let ((sub-tree (build-tree-r)))
                                 ;;(break)
                                 (push sub-tree tree)
                                 (adjust parsed-line)
                                 ))
                              ((< (nest-level parsed-line) (nest-level previous-parsed-line))
                               (push (list (html-tag parsed-line) (text parsed-line)) tree)
                               (when (zerop (nest-level parsed-line))
                                 (adjust parsed-line))
                               (return (reverse tree)))))
                       finally (return (reverse tree))))))
      (build-tree-r))))

(defun build-tree-old2 (parsed-lines)
  "Build a tree based on the parsed lines. Input: list of parsed-line. Output: tree of html tags suitable for feeding to cl-who."
  (let ((index 0))
    (labels ((build-tree-r ()
               (let ((tree))
                 (loop for previous-parsed-line = (car tree)
                       for parsed-line = (nth index parsed-lines)
                       while (< index (length parsed-lines))
                       do
                          (cond
                            ((or
                              (null tree)
                              (= (nest-level parsed-line) (nest-level previous-parsed-line)))
                             (push parsed-line tree))
                            ((> (nest-level parsed-line) (nest-level previous-parsed-line))
                             (let ((sub-tree (build-tree-r)))
                               (setf (sub-list previous-parsed-line) sub-tree)))
                            ((< (nest-level parsed-line) (nest-level previous-parsed-line))
                             (return tree)))
                          (incf index)
                       finally (return tree)))))
      (build-tree-r))))

(defun build-tree-old (parsed-lines tree)
  (loop for parsed-line in parsed-lines
        for previous-parsed-line = (car tree)
        for index = 0 then (incf index)
        do
        (cond
          ((null tree)
           ;; (break)
           (push parsed-line tree))
          ((= (nest-level parsed-line) (nest-level previous-parsed-line))
           ;; (break)
           (push parsed-line tree))
          ((> (nest-level parsed-line) (nest-level previous-parsed-line))
           (let ((sub-tree (build-tree (subseq parsed-lines index) (sub-list previous-parsed-line))))
             ;; (break)
             (setf (sub-list previous-parsed-line) sub-tree)))
          ((< (nest-level parsed-line) (nest-level previous-parsed-line))
           (return tree))))
  tree)

(defun parse-org-text-old2 (org-text)
  "Given a string of text, parse it. Input: text. Output: list suitable for use with something like cl-who."
  (let ((regex-scanners (make-scanners))
        (string (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
        (parsed-lines ()))
    (with-input-from-string (in org-text)
      (with-output-to-string (out string)
        (do ((i 1 (incf i))
             (line #1=(read-line in nil nil) #1#)
             (match-found nil))
            ((null line) (values (nreverse parsed-lines) (format nil "~%Original Lines:~%~A~%~D line~:P read." string (1- i))))
          (loop for regex-scanner in regex-scanners
                for scanner = (car regex-scanner)
                for element = (cdr regex-scanner)
                for previous-parsed-lines (car parsed-lines)
                do
                   (setq match-found nil)
                   (multiple-value-bind (match-start _match-end group-starts _group-ends)
                       (ppcre:scan scanner line)
                     (declare (ignore _match-end _group-ends))
                     (when match-start
                       (setq match-found t)
                       (let* ((group-index (1- (length group-starts)))
                              (text (subseq line (aref group-starts group-index)))
                              (nest-level (if (nest-level previous-parsed-lines) (if ))))
                         (push (make-instance 'parsed-line :text text :html-tag (html-tag element) :nest-level 1) parsed-lines))
                       (return))))
          (when (not match-found)
            (let ((previous-level (if (eql 'parsed-line (type-of (car parsed-lines))) (nest-level (car parsed-lines)) 1)))
             (push (make-instance 'parsed-line :text line :html-tag :span :nest-level previous-level) parsed-lines)))
          (format out "Line # ~D: ~A~%" i line))))))

(defun parse-org-text-old (org-text)
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

;; this works
(let ((my-html '((:html
                  (:body
                   (:H1 "Top") (:H2 "Next Level") (:SPAN "Just some normal text here.")
                   (:H3 "Nexter Level")
                   (:UL
                    (:LI "Item 1")
                    (:UL
                     (:LI "Sub-Item 1"))
                    (:LI "Item 2"))
                   (:H4 "Even nexter level")
                   (:UL
                    (:LI "Item 3")
                    (:UL
                     (:LI "Sub-Item 4")
                     (:UL (:LI "Sub-sub-Item 5")))
                    (:LI "Item 6")))))))
  (eval `(who:with-html-output-to-string
             (stream nil :prologue t :indent t)
           ,@my-html
           )))

(defun who-test ()
  (let ((my-html (build-tree (parse-org-text *test-text*))))
    (eval `(who:with-html-output-to-string
               (stream nil :prologue t :indent t)
             ,@my-html))))

(defun who-test-old ()
  (let ((my-html (build-tree (parse-org-text *test-text*))))
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
