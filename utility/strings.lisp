;;;; string utility
(in-package #:jfh-utility)

(defun string-starts-with (search string)
  "Does the beginning of string match the search string?"
  (search search string :start2 0 :end2 (length search)))

(defun trim-space (string)
  "Trim spaces from a string."
  (string-trim '(#\Space) string))

(defun empty-string-p (string)
  "Predicate tests if string is zero-length"
  (zerop (length string)))
