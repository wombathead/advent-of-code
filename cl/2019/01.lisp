
(load "util.lisp")

(defun advent-01a (filename)
  (let ((input (mapcar #'parse-integer (get-file filename))))
    (loop while input
          for m = (pop input)
          for f = (- (floor m 3) 2)
          if (plusp f)
          sum f)))

(defun advent-01b (filename)
  (let ((input (mapcar #'parse-integer (get-file filename))))
    (loop while input
          for m = (pop input)
          for f = (- (floor m 3) 2)
          when (plusp f)
          sum f
          and do (push f input))))

