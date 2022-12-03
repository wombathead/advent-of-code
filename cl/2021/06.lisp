
;; ----------------------
;;   Day 6: Lanternfish 
;; ----------------------

(in-package :aoc)

(defun step-population-list (population)
  (loop for f in population
        if (zerop f)
        collect 6 into g and collect 8 into h
        else
        collect (1- f) into g
        finally (return (append g h))))

(defun step-population-array (population)
  (loop with n = (length population)
        with p = (make-array (list n))
        with new-additions = (aref population 0)
        for i from 0 below (1- n)
        do (setf (aref p i) (aref population (1+ i)))
        finally
        (incf (aref p 6) new-additions)
        (incf (aref p 8) new-additions)
        (return p)))

(defun aoc-2021-06a (filename)
  (let ((input (mapcar #'parse-integer (str:split "," (first (read-from-file filename))))))
    (loop for i from 0 upto 80
          for f = input then (step-population-list f)
          finally (return (length f)))))

(defun aoc-2021-06b (filename)
  (let ((input (mapcar #'parse-integer (str:split "," (first (read-from-file filename)))))
        (population (make-array '(9))))
    (loop for i from 0 upto 8
          do (setf (aref population i) (count i input)))
    (loop for i from 0 upto 256
          for f = population then (step-population-array f)
          finally (return (reduce #'+ f)))))
