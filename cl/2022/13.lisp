;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 13: Distress Signal ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun packet= (xs ys)
  "Return 1 if XS < YS, 0 if XS = YS, and -1 if XS > YS"
  (let ((x (first xs))
        (y (first ys)))
    (cond ((and (null xs) (null ys)) 0)
          ((null xs)  1)
          ((null ys) -1)

          ((and (numberp x) (numberp y))
           (let ((r (packet= (rest xs) (rest ys))))
             (if (= x y)
                 (if (zerop r)
                     (packet= (rest (rest xs)) (rest (rest ys)))
                     r)
                 (if (< x y) 1 -1))))

          ((and (listp x) (listp y))
           (let ((r (packet= x y)))
             (if (zerop r)
                 (packet= (rest xs) (rest ys))
                 r)))

          (t
           (let* ((x* (if (numberp x) (list x) x))
                  (y* (if (numberp y) (list y) y))
                  (r (packet= x* y*)))
             (if (zerop r)
                 (packet= (rest xs) (rest ys))
                 r))))))

(defun packet< (xs ys)
  (plusp (packet= xs ys)))

(defun read-pairs-from-file (filename)
  (loop for line in (read-from-file filename)
        unless (empty-string-p line)
        collect (read-from-string (multiple-regex-replace
                                    line '("," "\\[" "\\]") '(" " "(" ")")))))

(defun aoc-2022-13a (filename)
  (loop for pair on (read-pairs-from-file filename) by #'cddr
        for x = (first pair)
        for y = (first (rest pair))
        for i from 1
        if (plusp (packet= x y))
        sum i))

(defun aoc-2022-13b (filename)
  (iter:iter
    (iter:with a = '((2)))
    (iter:with b = '((6)))
    (iter:with pairs = (append (read-pairs-from-file filename) (list a b)))
    (iter:for p in (sort pairs #'packet<))
    (iter:for i from 1)
    (if (or (equal p a) (equal p b))
        (iter:multiply i))))
