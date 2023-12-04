;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 1: Trebuchet?! ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun aoc-2023-01a (filename)
  (loop for line in (read-from-file filename)
        for digits = (mapcar #'digit-char-p (remove-if-not #'digit-char-p (coerce line 'list)))
        for ldigit = (first digits) and rdigit = (first (last digits))
        sum (+ (* 10 ldigit) rdigit)))

(defun description->digit (string)
  (let* ((words '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
         (digit-strings '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
         (digits (loop for i from 1 upto 9 collect i)))
    (cdr (assoc string (pairlis (append words digit-strings) (append digits digits)) :test 'string=))))

(defun search-leftmost-description (line descriptions)
  (loop with n = (length line)
        with min-idx = (1+ n)
        with leftmost
        for description in descriptions
        for idx = (search description line)
        when (and idx (< idx min-idx))
          do (setf min-idx idx
                   leftmost (subseq line idx (+ idx (length description))))
        finally (return leftmost)))

(defun search-rightmost-description (line descriptions)
  (let ((reversed-descriptions (mapcar #'reverse descriptions))
        (reversed-line (reverse line)))
    (reverse (search-leftmost-description reversed-line reversed-descriptions))))

(defun aoc-2023-01b (filename)
  (let ((descriptions '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
    (loop for line in (read-from-file filename)
          for ldigit = (description->digit (search-leftmost-description line descriptions))
          for rdigit = (description->digit (search-rightmost-description line descriptions))
          sum (+ (* 10 ldigit) rdigit))))
