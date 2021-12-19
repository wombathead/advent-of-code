
;; ----------------------------------
;;   Day 7: The Treachery of Whales
;; ----------------------------------

(load "util.lisp")

(defun advent-07a (filename)
  (flet ((total-cost (position numbers)
           "Sum of absolute differences between POSITION and NUMBERS"
           (loop for n in numbers
                 sum (abs (- position n)))))
    (loop with input = (get-numbers filename)
          with m = (median input)
          for y in (if (integerp m) (list m) (list (floor m) (ceiling m)))
          minimize (total-cost y input))))

(defun advent-07b (filename)
  (flet ((total-cost (position numbers)
           (loop for n in numbers
                 for difference = (abs (- position n))
                 sum (/ (* difference (1+ difference)) 2))))
    (loop with input = (get-numbers filename) 
          for i from (reduce #'min input) upto (reduce #'max input)
          minimize (total-cost i input))))
