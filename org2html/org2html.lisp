(cl:in-package #:org2html)

(defclass element ()
  ((%pattern :reader pattern
             :initarg :pattern)
   (%html-tag :reader html-tag
              :initarg :html-tag)
   (%is-item :reader is-item
             :initarg :is-item
             :initform nil)))

(defparameter *elements* (list
                          (make-instance 'element :pattern "^(\\s+)?\\-\\s+(\\w+)" :html-tag :li :is-item t)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\s+(\\w+)" :html-tag :h1)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\s+(\\w+)" :html-tag :h2)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\*\\s+(\\w+)" :html-tag :h3)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\*\\*\\s+(\\w+)" :html-tag :h4)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\*\\*\\*\\s+(\\w+)" :html-tag :h5)
                          (make-instance 'element :pattern "^[\\s+]?\\*\\*\\*\\*\\*\\*\\s+(\\w+)" :html-tag :h6)))

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

(defun parse-org-text (org-text &optional formatter)
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
            (let ((text (if formatter (funcall formatter line) line)))
              (push (make-instance 'parsed-line :text text :html-tag :span) parsed-lines)))
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

(defun who-test (&optional (text *test-text*))
  (let ((my-html (build-tree (parse-org-text text))))
    (eval `(who:with-html-output-to-string
               (stream nil :prologue t :indent t)
             (:html
              (:body
               (:div
                ,@my-html)))))))

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

(defun save-as-html (html)
  "Saves html to an html file."
  (let ((path "./html/show.html"))
    (with-open-file (out path  :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out html))))
