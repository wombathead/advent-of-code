;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 1: Historian Hysteria ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:aoc)

(flet ((to-numbers (filename)
         (loop for line in (read-from-file filename)
               collect (mapcar #'parse-integer (str:words line)))))

  (defun aoc-2024-01a (filename)
    (loop with input = (to-numbers filename)
          for l in (sorted (slice 0 input) #'>)
          for r in (sorted (slice 1 input) #'>)
          sum (abs (- l r))))


  (defun aoc-2024-01b (filename)
    (loop with input = (to-numbers filename)
          with left-list = (sorted (slice 0 input) #'>)
          with right-list = (sorted (slice 1 input) #'>)
          for target in left-list
          sum (* target (count target right-list)))))
