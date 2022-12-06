;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 6: Tuning Trouble ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun members-until-window-distinct (list window-size)
  (loop for start on list
        for window = (subseq start 0 window-size)
        for i from window-size
        if (= window-size (length (remove-duplicates window)))
        return i))

(defun aoc-2022-06a (filename)
  (members-until-window-distinct
    (coerce (first (read-from-file filename)) 'list) 4))

(defun aoc-2022-06b (filename)
  (members-until-window-distinct
    (coerce (first (read-from-file filename)) 'list) 14))
