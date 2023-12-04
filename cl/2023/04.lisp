;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 4: Scratchcards ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun parse-card (line)
  (destructuring-bind
      (left right) (ppcre:split "\\| " (subseq line (nth-value 1 (ppcre:scan "Card +\\d+: " line))))

    (list (mapcar #'parse-integer (str:words left))
          (mapcar #'parse-integer (str:words right)))))

(defun aoc-2023-04a (filename)
  (loop for line in (read-from-file filename)
        for (left right) = (parse-card line)
        sum (let ((intersection (intersection left right)))
              (if intersection
                  (expt 2 (1- (length intersection)))
                  0))))

(defun aoc-2023-04b (filename)
  (loop with instances = (make-array (length (read-from-file filename)) :initial-element 1)
        for line in (read-from-file filename)
        for (left right) = (parse-card line)
        for i from 0
        for winning-numbers = (length (intersection left right))
        do (loop for j from (1+ i) upto (+ i winning-numbers)
                 do (incf (aref instances j) (aref instances i)))
        finally (return (reduce #'+ instances))))
