;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 3: Mull It Over ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:aoc)


(flet ((concatenate-lines (filename)
         (reduce (lambda (r s) (concatenate 'string r s)) (read-from-file filename))))

  (defun aoc-2024-03a (filename)
    (let ((input (concatenate-lines filename))
          (result 0))
      (ppcre:do-register-groups ((#'parse-integer x y))
          ("mul\\((\\d+),(\\d+)\\)" input)
        (incf result (* x y)))
      result))


    (defun aoc-2024-03b (filename)
      (iter
        (with input = (concatenate-lines filename))
        (with enable = "do()")
        (with disable = "don't()")
        (with flag = t)

        (for match in (ppcre:all-matches-as-strings "do\\(\\)|don't\\(\\)|mul\\(\\d+,\\d+\\)" input))
        (for computation? = (not (or (string= match enable) (string= match disable))))

        (when (string= match enable) (setf flag t))
        (when (string= match disable) (setf flag nil))

        (when (and flag computation?)
          (ppcre:register-groups-bind ((#'parse-integer x y))
              ("mul\\((\\d+),(\\d+)\\)" match)
            (sum (* x y)))))))
