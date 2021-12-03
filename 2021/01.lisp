
;; ----------------------
;;   Day 1: Sonar Sweep
;; ----------------------

(load "util.lisp")

(defun advent-01a (filename)
  (loop with input = (mapcar #'parse-integer (get-file filename))
        for a = nil then b
        for b in input
        when a
        count (> b a)))

(defun advent-01b (filename)
  (loop with input = (mapcar #'parse-integer (get-file filename))
        for (a b c) on input
        while (and a b c)
        for prev-sum = nil then sum
        for sum = (+ a b c)
        when prev-sum
        count (> sum prev-sum)))
