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
         (previous-level 0)
         (counter 0))
    (loop for input in inputs
          for tag = (car input)
          for text = (cadr input)
          for level = (caddr input)
          while (<= (incf counter) (* 2 (length inputs)))
          do
             (cond
               ((> level previous-level)
                (push tree parents)
                (setf tree (car tree)))
               ((< level previous-level)
                (loop repeat (- previous-level level)
                      do
                         (push (reverse tree) (car (car parents)))
                         (setf tree (pop parents)))))
             (push (list tag text level) tree)
             (setf previous-level level)
             (when verbose
               (format t "~&***************Input: ~A~%Tree: ~A~%Parents: ~A~%" input tree parents)))
    (when verbose
      (format t "Counter: ~D" counter))
    (reverse tree)))
