
;; ----------------------------------
;;   Day 7: The Treachery of Whales
;; ----------------------------------

(load "util.lisp")

(defun advent-07a (filename)
  (flet ((total-cost (position numbers)
           "Sum of absolute differences between POSITION and NUMBERS"
           (loop for n in numbers
                 sum (abs (- position n)))))
    (let* ((input (get-numbers filename))
           (y (median input)))
      (total-cost y input))))

(defun advent-07b (filename)
  (flet ((total-cost (position numbers)
           (loop for n in numbers
                 for difference = (abs (- position n))
                 sum (/ (* difference (1+ difference)) 2))))
    (loop with input = (get-numbers filename) 
          for i from (reduce #'min input) upto (reduce #'max input)
          minimize (total-cost i input))))
