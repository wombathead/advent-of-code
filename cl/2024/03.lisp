;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 3: Mull It Over ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:aoc)


(defun aoc-2024-03a (filename)
  (iter
    (with input = (reduce (lambda (r s) (concatenate 'string r s)) (read-from-file filename)))
    (for match in (ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" input))
    (for (x y) = (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" match)))
    (sum (* x y))))


(defun aoc-2024-03b (filename)
  (iter
    (with input = (reduce (lambda (r s) (concatenate 'string r s)) (read-from-file filename)))
    (with enable = "do()")
    (with disable = "don't()")
    (with flag = t)

    (for match in (ppcre:all-matches-as-strings "(do\\(\\)|don't\\(\\)|mul\\(\\d+,\\d+\\))" input))
    (for computation? = (not (or (string= match enable) (string= match disable))))

    (when (string= match enable) (setf flag t))
    (when (string= match disable) (setf flag nil))

    (when (and flag computation?)
      (destructuring-bind (x y)
          (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" match))
        (sum (* x y))))))
