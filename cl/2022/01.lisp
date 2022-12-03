;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 1: Calorie Counting ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun top-n-bags (bags n)
  (subseq (sort (mapcar (lambda (b) (reduce #'+ b)) bags) #'>) 0 n))

(defun aoc-2022-01a (filename)
  (loop with current-bag and bags
        for line in (read-from-file filename)
        if (empty-string-p line)
        do (progn (push current-bag bags)
                  (setf current-bag nil))
        else
        do (push (parse-integer line) current-bag)
        finally (return (first (top-n-bags bags 1)))))

(defun aoc-2022-01b (filename)
  (loop with current-bag and bags
        for line in (read-from-file filename)
        if (empty-string-p line)
        do (progn (push current-bag bags)
                  (setf current-bag nil))
        else
        do (push (parse-integer line) current-bag)
        finally (return (reduce #'+ (top-n-bags bags 3)))))
