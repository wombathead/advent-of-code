;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 6: Tuning Trouble ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun aoc-2022-06a (filename)
  (loop with buffer = (coerce (first (read-from-file filename)) 'list)
        for start on buffer
        for window = (subseq start 0 4)
        for i from 4
        if (= 4 (length (remove-duplicates window)))
        return i))

(defun aoc-2022-06b (filename)
  (loop with buffer = (coerce (first (read-from-file filename)) 'list)
        for start on buffer
        for window = (subseq start 0 14)
        for i from 14
        if (= 14 (length (remove-duplicates window)))
        return i))
