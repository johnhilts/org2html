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
