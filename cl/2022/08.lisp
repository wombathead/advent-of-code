;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 8: Treetop Tree House ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun tree-visible-p (matrix i j)
  (flet ((visible-direction-p (matrix y x dy dx)
           (loop with (n m) = (array-dimensions matrix)
                 with h = (aref matrix y x)
                 for yi = (+ y dy) then (+ yi dy)
                 for xi = (+ x dx) then (+ xi dx)
                 until (or (minusp yi) (minusp xi) (>= yi n) (>= xi m))
                 if (>= (aref matrix yi xi) h)
                 return nil
                 finally (return t))))

    (member t (mapcar (lambda (dy dx) (visible-direction-p matrix i j dy dx))
                '(-1 0 1  0)
                '( 0 1 0 -1)))))

(defun aoc-2022-08a (filename)
  (loop with trees = (read-number-grid filename)
        with (n m) = (array-dimensions trees)
        for i from 0 below n
        sum (loop for j from 0 below m
                  count (tree-visible-p trees i j))))

(defun viewing-distance (matrix y x dy dx)
  (loop with (n m) = (array-dimensions matrix)
        with h = (aref matrix y x)
        for yi = (+ y dy) then (+ yi dy)
        for xi = (+ x dx) then (+ xi dx)
        count t
        until (or (<= yi 0) (<= xi 0)
                  (>= yi (1- n)) (>= xi (1- m))
                  (>= (aref matrix yi xi) h))))

(defun scenic-score (matrix i j)
  (reduce #'* (mapcar (lambda (dy dx) (viewing-distance matrix i j dy dx))
                      '(-1 0 1  0)
                      '( 0 1 0 -1))))

(defun aoc-2022-08b (filename)
  (loop with trees = (read-number-grid filename)
        with (n m) = (array-dimensions trees)
        for i from 0 below n
        maximize (loop for j from 0 below m
                       maximize (scenic-score trees i j))))

