;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 6: Wait For It ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun number-of-feasible-solutions-analytical (duration record)
  (let ((t0 (* 1/2 (- duration (sqrt (- (expt duration 2) (* 4 record))))))
        (t1 (* 1/2 (+ duration (sqrt (- (expt duration 2) (* 4 record)))))))
    (- (ceiling t1) (floor t0) 1)))

(defun aoc-2023-06a (filename)
  (let* ((input (read-from-file filename))
         (durations (mapcar #'parse-integer (rest (str:words (first input)))))
         (records (mapcar #'parse-integer (rest (str:words (second input))))))
    (reduce #'* (mapcar (lambda (duration record) (number-of-feasible-solutions-procedural duration record)) durations records))))

(defun number-of-feasible-solutions-procedural (duration record)
  (flet ((distance-travelled (time)
           (* time (- duration time))))
    (loop with t* = (floor duration 2)
          for i from 1
          for t- = (- t* i) and t+ = (+ t* i)
          for d- = (distance-travelled t-) and d+ = (distance-travelled t+)
          count (> d- record) into left
          count (> d+ record) into right
          until (and (<= d- record) (<= d+ record))
          finally (return (+ left right 1)))))

(defun aoc-2023-06b (filename)
  (let* ((input (read-from-file filename))
         (duration (parse-integer (str:join "" (rest (str:words (first input))))))
         (record (parse-integer (str:join "" (rest (str:words (second input)))))))
    (number-of-feasible-solutions-procedural duration record)))
