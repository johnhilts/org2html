(cl:in-package #:org2html)

(defparameter *has-header-line* nil)

(defclass element ()
  ((%pattern :reader pattern
             :initarg :pattern)
   (%html-tag :reader html-tag
              :initarg :html-tag)
   (%is-item :reader is-item
             :initarg :is-item
             :initform nil)
   (%is-table :reader is-table
             :initarg :is-table
             :initform nil)
   (%is-code :reader is-code
             :initarg :is-code
             :initform nil)))

(defparameter *displayable-text-pattern* "\\+\\*\\$\\\"\\w"
  "Subset of characters that are acceptable as text. Regex format.")

(defparameter *elements*
  (list
   (make-instance 'element :pattern "^[\\s+]?(\\|.+\\|)[\\s+]?$" :html-tag :tr :is-table t)
   (make-instance 'element :pattern "^[\\s+]?\\#\\+begin_src\\s(\\w+)" :html-tag :code :is-code t)
   (make-instance 'element :pattern "^\\#\\+title:\\s(.+)" :html-tag :title)
   (make-instance 'element :pattern (format nil "^(\\s+)?\\- \\[ \\]\\s+([~A]+)" *displayable-text-pattern*) :html-tag :input)
   (make-instance 'element :pattern (format nil "^(\\s+)?\\-\\s+([~A]+)" *displayable-text-pattern*) :html-tag :li :is-item t)
   (make-instance 'element :pattern (format nil "^[\\s+]?\\*\\s+([~A]+)" *displayable-text-pattern*) :html-tag :h1)
   (make-instance 'element :pattern (format nil "^[\\s+]?\\*\\*\\s+([~A]+)" *displayable-text-pattern*) :html-tag :h2)
   (make-instance 'element :pattern (format nil "^[\\s+]?\\*\\*\\*\\s+([~A]+)" *displayable-text-pattern*) :html-tag :h3)
   (make-instance 'element :pattern (format nil "^[\\s+]?\\*\\*\\*\\*\\s+([~A]+)" *displayable-text-pattern*) :html-tag :h4)
   (make-instance 'element :pattern (format nil "^[\\s+]?\\*\\*\\*\\*\\*\\s+([~A]+)" *displayable-text-pattern*) :html-tag :h5)
   (make-instance 'element :pattern (format nil "^[\\s+]?\\*\\*\\*\\*\\*\\*\\s+([~A]+)" *displayable-text-pattern*) :html-tag :h6)))

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
                :initform 0))
  (:documentation "The results of a parsed line."))

(defmethod print-object ((parsed-line parsed-line) stream)
  "Print parsed line."
  (print-unreadable-object (parsed-line stream :type t)
    (with-accessors ((text text)
                     (html-tag html-tag)
                     (nest-level nest-level))
        parsed-line
      (format stream
	      "Text: ~S, HTML Tag: ~S~:[~:;, Nest Level: ~:*~D~]"
	      text html-tag nest-level))))

(defun parse-source-code-block (in line tag)
  "Parse source code block. Input: input stream and parsed lines. First line of source code block. Output: parsed lines."
  (let ((output-string (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (out output-string)
      (do
       (end-of-block)
       ((or
         (null line)
         end-of-block)
        (make-instance 'parsed-line :html-tag tag :text output-string :nest-level 1)) ;; todo: capture language name here

        (setf line (read-line in nil nil))
        (when (search "#+end_src" line)
          (setf end-of-block t))
        (unless end-of-block
          (format out "~A~%" (tbnl:escape-for-html line)))))))

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

;; todo get language from a dictionary that maps org language names to highlight.js names
(defun parse-org-source-code-block (in line element code-language parsed-lines)
  "Parse source code block.
Input: input stream, current line, parsed element, code-language, parsed lines so far.
Output: all parsed lines, including new ones added here."
  (push (make-instance 'parsed-line :text code-language :html-tag :code-language) parsed-lines)
  (push (make-instance 'parsed-line :text "" :html-tag :pre) parsed-lines)
  (push (parse-source-code-block in line (html-tag element)) parsed-lines)
  parsed-lines)

(defun parse-table-row (element text previous-parsed-line parsed-lines)
  "Parse table row.
Input: parsed element, parsed text, level, previous parsed line (from previous iteration), parsed lines so far.
Output: all parsed lines, including new ones added here."
  (flet ((row-parser (text)
           (mapcar
            #'jfh-utility:trim-space
            (remove-if
             #'jfh-utility:empty-string-p
             (ppcre:split "\\|" text)))))
    (when (or
           (null previous-parsed-line)
           (zerop (nest-level previous-parsed-line)))
      (setf *has-header-line* nil)
      (push (make-instance 'parsed-line :text "" :html-tag :table :nest-level 0) parsed-lines))
    (if (and (not *has-header-line*) (ppcre:scan (ppcre:create-scanner "^[|\\-\\+]+$") text))
        (setf *has-header-line* t)
        (progn
              (push (make-instance 'parsed-line :text "" :html-tag (html-tag element) :nest-level 1) parsed-lines)
              (loop for column in (row-parser text)
                    do
                       (push (make-instance 'parsed-line :text column :html-tag (if *has-header-line* :td :th) :nest-level 2) parsed-lines)))))
  parsed-lines)

(defun parse-nested-element (element text nest-level group-end previous-parsed-line parsed-lines)
  "Parse nested element (list item).
Input: parsed element, parsed text, nest level, match group end, previous parsed line (from previous iteration), parsed lines so far.
Output: all parsed lines, including new ones added here."
  (flet ((increase-nest-level-p ()
           (or
            (null previous-parsed-line)
            (zerop (nest-level previous-parsed-line))
            (< (nest-level previous-parsed-line) nest-level))))
    (when (increase-nest-level-p)
      (push (make-instance 'parsed-line :text "" :html-tag :ul :nest-level (nest-level previous-parsed-line)) parsed-lines)))
  (push (make-instance 'parsed-line :text text :html-tag (html-tag element) :match-group-end group-end :nest-level nest-level) parsed-lines)
  parsed-lines)

(defun parse-org-text (org-text &optional formatter)
  "Given a string of text, parse it. Input: text. Output: list suitable for use with something like cl-who."
  (let ((regex-scanners (make-scanners))
        (parsed-lines ()))
    (with-input-from-string (in org-text)
      (do ((i 1 (incf i))
           (line #1=(read-line in nil nil) #1#)
           (match-found nil))
          ((null line) (nreverse parsed-lines))
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
                     (if (is-code element)
                         (setf parsed-lines (parse-org-source-code-block in line element (subseq line (aref group-starts 0) (aref group-ends 0)) parsed-lines))
                         (let* ((is-list-item (eql (html-tag element) :li))
                                (group-end (if is-list-item (if (null (aref group-ends 0)) 0 (aref group-ends 0)) 0))
                                (is-table  (is-table element))
                                (nest-level
                                  (if (eql (html-tag element) :li)
                                      (if previous-parsed-line (get-nest-level previous-parsed-line group-end) nil)
                                      (if is-table 1 nil)))
                                (group-index (1- (length group-starts)))
                                (raw-text (subseq line (aref group-starts group-index)))
                                (text (if formatter (funcall formatter raw-text) raw-text)))
                           (if nest-level
                               (if is-table
                                   (setf parsed-lines (parse-table-row element text previous-parsed-line parsed-lines))
                                   (setf parsed-lines (parse-nested-element element text nest-level group-end previous-parsed-line parsed-lines)))
                               (push (make-instance 'parsed-line :text text :html-tag (html-tag element)) parsed-lines))))
                     (return))))
        (when (not match-found)
          (let ((text (if formatter (funcall formatter line) line)))
            (push (make-instance 'parsed-line :text text :html-tag :span) parsed-lines)))))))

(defun format-urls (string)
  "Format text with URLs as a cl-who sexp.
Input: text.
Output: cl-who sexp of (:span text) or if URLs are found then (:span (:a :href \"url\" url))"
  (flet ((format-url (part-before-match-start match-start the-match)
           "Format 1 URL.
Input: part of target string preceding matching part, match start position, matching part of string.
Output: cl-who list of string + anchor tag surrounding URL."
           (list (subseq string part-before-match-start match-start)
                 (list :a :href (format nil "~A" the-match) the-match))))
    (let ((regex "(?:http|https)://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,3}(/\\S*)?"))
      (do
       ((i 0)
        (no-matches)
        (results ())
        (next-start-pos 0))
       ((or
         (>= i (length string))
         no-matches)
        (cond
          ((>= next-start-pos (length string)) results)
          (t (append results (list (subseq string next-start-pos))))))
        (multiple-value-bind
              (match-start match-end group-starts group-ends)
            (cl-ppcre:scan regex string :start i)
          (declare (ignore group-starts group-ends))
          (when match-start
            (let ((the-match (subseq string match-start match-end)))
              (setf results (append results (format-url next-start-pos match-start the-match)))
              (setf next-start-pos match-end)
              (setf i (1+ match-end))))
          (unless match-start
            (setf no-matches t)))))))

(defun run-format-urls-tests ()
  (let ((list ()))
    (let ((s "Search on https://www.google.com, then veg-out on https://www.youtube.com")) (format t "~S: ~S~%~%**********" s  (push (format-urls s) list)))
    (let ((s "Search on https://www.google.com - it's the best"))(format t "~S: ~S~%~%**********" s (push (format-urls s) list)))
    (let ((s "Pick your favorite: https://www.google.com, https://www.youtube.com")) (format t "~S: ~S~%~%**********" s (push (format-urls s) list)))
    (nreverse list)))

(defparameter *header-tags*
  (list :title :script :link :code-language))

(defun build-tree (parsed-lines &optional (verbose nil))
  (flet ((complete-tag (tag text)
           (let ((formatted-text (format-urls text)))
             (cond
               ((eql :ul tag) (list :ul))
               ((eql :input tag) (apply #'list `(:input :type "checkbox" ,@formatted-text :br)))
               ((eql :table tag) (reverse (apply #'list `(:table :border "3" :cellspacing "3")))) ;; todo why does just this one have to be forth style??
               (t (apply #'list `(,tag ,@formatted-text)))))))
    (let* ((body-tree)
           (head-tree)
           (parents body-tree)
           (previous-level 0))
      (flet ((push-in ()
               (push body-tree parents)
               (setf body-tree (car body-tree)))
             (pop-out (times)
               (loop repeat times
                     do
                        (setf (car (car parents)) (reverse body-tree))
                        (setf body-tree (pop parents)))))
        (loop for parsed-line in parsed-lines
              for tag = (html-tag parsed-line)
              for text = (text parsed-line)
              for level = (nest-level parsed-line)
              do
                 (cond
                   ((> level previous-level)
                    (push-in))
                   ((< level previous-level)
                    (pop-out (- previous-level level))))
                 (if (member tag *header-tags*)
                     (push (complete-tag tag text) head-tree)
                     (push (complete-tag tag text) body-tree))
                 (setf previous-level level)
                 (when verbose
                   (format t "~&***************Input: ~A~%Tree: ~A~%Parents: ~A~%" parsed-line body-tree parents))
              finally (when (> level 0)
                        (pop-out level))))
      (list (cons 'html-head (reverse head-tree))
            (cons 'html-body (reverse body-tree))))))

(defun convert-to-jira-markup (parsed-line)
  (cond ; replace with CASE here?
    ((string= 'li (symbol-value (html-tag parsed-line)))
     (format nil "~A ~A~%" (make-string (nest-level parsed-line) :initial-element #\-) (text parsed-line)))
    ((string= 'span (html-tag parsed-line))
     (format nil "~A~%" (text parsed-line)))
    ((string= 'tr (html-tag parsed-line))
     (format nil "~%| "))
    ((string= 'th (html-tag parsed-line))
     (format nil "**~A** |" (text parsed-line)))
    ((string= 'td (html-tag parsed-line))
     (format nil "~A |" (text parsed-line)))
    ((string= 'input (html-tag parsed-line))
     (format nil "**TODO**: ~A~%" (text parsed-line)))
    ((string= 'code-language (html-tag parsed-line))
     (format nil "{code:~A}~%" (text parsed-line)))
    ((string= 'code (html-tag parsed-line))
     (format nil "~A~%{code}~%" (text parsed-line)))
    ((member (html-tag parsed-line) '(ul table pre) :test 'string=)
     "")
    (t
     (format nil "~%~(~A~). ~A~%" (html-tag parsed-line) (text parsed-line)))))

(defun build-tree-for-jira (&optional (text *test-text*))
  (format nil "~%~{~A~}"
          (mapcar
           (lambda (parsed-line)
             (convert-to-jira-markup parsed-line))
           (parse-org-text text))))

(defun who-test (&optional (text *test-text*))
  (let* ((all-html (build-tree (parse-org-text text)))
         (html-head (cdr (assoc 'html-head all-html)))
         (html-body (cdr (assoc 'html-body all-html))))
    (eval `(who:with-html-output-to-string
               (stream nil :prologue t :indent t)
             (:html
              (:head ,@html-head)
              (:body
               (:div
                ,@html-body)))))))

(defun who-test-table (&optional (text *test-text*) (table-text *test-text-table*))
  (who-test (concatenate 'string text table-text)))

(defparameter *test-text-table*
  "
| Type                                 |   Fat | 1/4 % |
|--------------------------------------+-------+-------|
| Royal Canin Renal Support Cat Food T |  4.5% |       |
| Royal Canin Renal Support Cat Food E |  5.0% |   2.5 |
| Royal Canin Renal Support Cat Food D |  6.5% |       |
| Royal Canin Renal Support Cat Food F |   15% |  3.75 |
| Royal Canin Renal Support Cat Food A |   15% |  3.75 |
| Royal Canin Renal Support Cat Food S | 19.5% | 4.875 |
| Hills Tuna                           | 24.4% |       |
| Hills Chicken                        | 24.5% |       |")

(defparameter *test-text* "#+title: The Test Document
* Top
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
- Item 6
- [ ] Checkbox 1
* Example Code
#+begin_src lisp
  (defun add (n1 n2)
    (let ((n3 (exp n1 n2)))
      (list n1 n2 n3)))
#+end_src
** Example text
blah blah blah")

(defun test-row-parser (&optional (text *test-text-table*))
  (let ((org-text text))
    (let ((regex-scanners (make-scanners))
          (parsed-lines (loop for x in '(1 2 3) collect (make-instance 'parsed-line :text (format nil "~D" x) :html-tag :span))))
      (with-input-from-string (in org-text)
        (do ((i 1 (incf i))
             (line #1=(read-line in nil nil) #1#))
            ((null line) (nreverse parsed-lines))
          (loop for regex-scanner in regex-scanners
                for scanner = (car regex-scanner)
                for element = (cdr regex-scanner)
                for previous-parsed-line = (car parsed-lines)
                do
                   (multiple-value-bind (match-start _match-end group-starts group-ends)
                       (ppcre:scan scanner line)
                     (declare (ignore _match-end group-ends))
                     (when match-start
                       (let* ((group-index (1- (length group-starts)))
                              (text (subseq line (aref group-starts group-index))))
                         (setf parsed-lines (parse-table-row element text previous-parsed-line parsed-lines)))
                       (return)))))))))


(defun save-as-html (html)
  "Saves html to an html file."
  (let ((path "./html/show.html"))
    (with-open-file (out path  :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string html out))))
