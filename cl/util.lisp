;;;; utility functions

(in-package :aoc)

(defun read-from-file (filename)
  "Return contents of FILENAME as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))

(defun empty-string-p (string)
  (zerop (length string)))
