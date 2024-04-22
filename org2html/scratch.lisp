(cl:in-package #:org2html)

;; original
'((h1 blah )
  (h2 blah )
  (ul
   (li 1)
   (li 2))
  (h3 blah )
  (ul
   (li 3)
   (ul
    (li 4)
    (ul
     (li 5))
    (li 6)))
  (h4 bbb ))

;; lisp-ified
(defparameter *my/list*
  '((h1 blah )
    (h2 blah )
    (ul
     (li 1)
     (li 2))
    (h3 blah )
    (ul
     (li 3)
     (ul
      (li 4)
      (ul
       (li 5))
      (li 6)))
    (h4 bbb )))

;; now create the list from scratch
(defparameter *my/input*
  '((h1 blah 0)
    (h2 blah 0)
    (ul nil 0)
    (li 1 1)
    (li 2 1)
    (h3 blah 0)
    (ul nil 0)
    (li 3 1)
    (ul nil 1)
    (li 4 2)
    (ul nil 2)
    (li 5 3)
    (li 6 2)
    (h4 bbb 0)))

(defun create-list-from-scratch (&optional (verbose nil) (inputs *my/input*))
  (let* ((tree)
         (parents tree)
         (previous-level 0))
    (loop for input in inputs
          for tag = (car input)
          for text = (cadr input)
          for level = (caddr input)
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
             (push (list tag text level) tree)
             (setf previous-level level)
             (when verbose
               (format t "~&***************Input: ~A~%Tree: ~A~%Parents: ~A~%" input tree parents)))
    (reverse tree)))


(defun replace-urls (string)
  (let ((regex "(?:http|https)://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,3}(/\\S*)?")
        (adjust-length 0)
        (anchor-start "<a href=\"")
        (anchor-end "</a>"))
    (do
     ((i 0)
      (no-matches)
      (results string))
     ((or
       (>= i (length string))
       no-matches)
      results)
      (multiple-value-bind
            (match-start match-end group-starts group-ends)
          (cl-ppcre:scan regex string :start i)
        (declare (ignore group-starts group-ends))
        (when match-start
          (let ((the-match (subseq string match-start match-end)))
            (setf results (format nil "~A~A~A\">~A~A~A"
                                  (subseq results 0 (+ match-start adjust-length))
                                  anchor-start
                                  the-match
                                  the-match
                                  anchor-end
                                  (subseq string match-end)))
            (setf adjust-length (+ 2 (apply #'+ (mapcar #'length (list anchor-start anchor-end the-match)))))
            (setf i (1+ match-start))
            (format t "match: ~A~%" the-match)))
        (unless match-start
          (setf no-matches t))))))

(replace-urls "Search on https://www.google.com, then veg-out on https://www.youtube.com")
(replace-urls "Search on https://www.google.com - it's the best")
(replace-urls "Pick your favorite: https://www.google.com, https://www.youtube.com")


;;; parse an org table
(defun parse-org-table-row (org-text)
  (let* ((pattern "^[\\s+]?\\|.+\\|[\\s+]?$")
         (scanner (ppcre:create-scanner pattern)))
    (ppcre:scan scanner org-text)))

(defun table-tester ()
  (flet ((line-tester (line)
           (format t "~&Pattern: ~S yields ~{~A~^, ~}~%~%" line (multiple-value-list (parse-org-table-row line)))))
    (line-tester "| col1 | col2 |")
    (line-tester " | col1 | col2 |")
    (line-tester "not a table | col1 | col2 |")
    (line-tester "|-------+---------------+---|")))

(defun trim-space (string)
  "Trim spaces from a string."
  (string-trim '(#\Space) string))

(defun empty-string-p (string)
  "Predicate tests if string is zero-length"
  (zerop (length string)))

(defun row-parser (org-row-text)
  "Input: org table row text; Output: list of columns (td)"
  (mapcar
   'trim-space
   (remove-if
    'empty-string-p
    (ppcre:split "\\|" org-row-text))))

(defun table-parser (org-table-text)
  "Input: org table text - stored as a sequence; Output: list of rows (tr) containing columns (td)"
  (loop for text in org-table-text
        collect
        (row-parser text)))

(defun org-table-tester ()
  (let ((org-table-text '(
                          "| Type                                 |   Fat | 1/4 % |"
                          "|--------------------------------------+-------+-------|"
                          "| Royal Canin Renal Support Cat Food T |  4.5% |       |"
                          "| Royal Canin Renal Support Cat Food E |  5.0% |   2.5 |"
                          "| Royal Canin Renal Support Cat Food D |  6.5% |       |"
                          "| Royal Canin Renal Support Cat Food F |   15% |  3.75 |"
                          "| Royal Canin Renal Support Cat Food A |   15% |  3.75 |"
                          "| Royal Canin Renal Support Cat Food S | 19.5% | 4.875 |"
                          "| Hills Tuna                           | 24.4% |       |"
                          "| Hills Chicken                        | 24.5% |       |")))
    (table-parser org-table-text)))

;; todo add logic to track whether we've it the line yet; also flag whether a line actually denotes a header in the first place;
;; before hitting the line use <th>, afterwords <td> - don't forget <thead> <tbody> <tfoot> and friends either!
(defun org-table-2-html (parsed-rows)
  "Input: a sequence of rows, which are themselves a sequence of columns. Output: a sequence of cl-who html tags"
  (let* ((scanner (ppcre:create-scanner "^[\\-\\+]+$"))
         (has-header (find-if (lambda (row) (some (lambda (e) (ppcre:scan scanner e)) row)) parsed-rows))
         (passed-header nil))
    (append
     '(:table)
     (remove-if
      (lambda (e)
        (null (cddr e)))
      (loop for parsed-row in parsed-rows
            collect
            (append
             '(:tr)
             (loop for parsed-column in parsed-row
                   collect
                   (append
                    (if (or (not has-header) passed-header) '(:td) '(:th))
                    (if (ppcre:scan scanner parsed-column)
                        (progn
                          (setf passed-header t)
                          nil)
                        (list parsed-column))))))))))

(defun org-table-2-html-OLD (parsed-rows)
  "Input: a sequence of rows, which are themselves a sequence of columns. Output: a sequence of cl-who html tags"
  (let ((table '(:table)))
    (loop for parsed-row in parsed-rows
          with row = (list :tr)
          do
             (format t "Row: ~A~%" parsed-row)
             (loop for parsed-column in parsed-row
                   and column = (list :td)
                   do
                      (format t "Column: ~A~%" parsed-column)
                      (push parsed-column column)
                      (push column row))
             (push row table))
    table))

(defun tester ()
  (let ((noticed '())
        (bytes '()))
    (flet ((notice (x) (push x noticed) x))
      (with-input-from-string (s "test.bin")
        (notice (file-position s)) ;1
        (push (read-char s) bytes)
        (push (read-char s) bytes)
        (push (read-char s) bytes)
        (notice (file-position s)) ;4
        (notice (file-position s 1)) ;; set to position 1
        (notice (file-position s))
        (push (read-char s) bytes)
        (notice (file-position s)))
      (values
       (nreverse noticed)
       (nreverse bytes)))))
