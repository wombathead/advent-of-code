;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 3: Rucksack Reorganization ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun item-score (item)
  (if (lower-case-p item)
      (+ 1 (char-difference item #\a))  
      (+ 27 (char-difference item #\A))))

(defun aoc-2022-03a (filename)
  (loop for rucksack in (read-from-file filename)
        for n = (length rucksack)
        for left = (coerce (subseq rucksack 0 (floor n 2)) 'list)
        for right = (coerce (subseq rucksack (floor n 2)) 'list)
        for common = (remove-duplicates (intersection left right))
        sum (item-score (first common))))

(defun aoc-2022-03b (filename)
  (loop for remaining on (read-from-file filename) by #'cdddr
        while (> (length remaining) 2)
        for rucksacks = (mapcar (lambda (s) (coerce s 'list)) (subseq remaining 0 3))
        for common = (reduce #'intersection (mapcar #'remove-duplicates rucksacks))
        sum (item-score (first common))))
