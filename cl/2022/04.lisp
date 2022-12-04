;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 4: Camp Cleanup ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun interval-fully-contains-p (a b c d)
  (and (<= a c) (>= b d)))

(defun interval-overlaps-p (a b c d)
  "Return T if [a,b] overlaps with "
  (and (<= a d) (>= b c)))

(defun aoc-2022-04a (filename)
  (loop for line in (read-from-file filename)
        for (a b c d) = (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line))
        count (or (interval-fully-contains-p a b c d) (interval-fully-contains-p c d a b))))

(defun aoc-2022-04b (filename)
  (loop for line in (read-from-file filename)
        for (a b c d) = (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line))
        count (interval-overlaps-p a b c d)))
