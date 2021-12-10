
;; ----------------------------------
;;   Day 7: The Treachery of Whales
;; ----------------------------------

(load "util.lisp")

(defun advent-07a (filename)
  (flet ((total-cost (position numbers)
           "Sum of absolute differences between POSITION and NUMBERS"
           (loop for n in numbers
                 sum (abs (- position n)))))
    (let* ((numbers (get-numbers filename))
           (y (median numbers)))
      (loop for y in (if (integerp y) (list y) (list (floor y) (ceiling y)))
        minimize (total-cost y numbers)))))

(defun advent-07b (filename)
  (flet ((total-cost (position numbers)
           (loop for n in numbers
                 for difference = (abs (- position n))
                 sum (/ (* difference (1+ difference)) 2))))
    (let ((input (get-numbers filename)))
      (loop for i from (reduce #'min input) upto (reduce #'max input)
        minimize (total-cost i input)))))
