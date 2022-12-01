;; ---------------------------- ;;
;;    DAY 1 - REPORT REPAIR     ;;
;; ---------------------------- ;;

(load "util.lisp")

(defun prepare-input (input)
  (sort (remove-duplicates (mapcar #'parse-integer (get-file input))) #'<))

(defun day1a (input)
  "find the X and Y from input such that their sum is 2020 and return X*Y"
  (let ((input (prepare-input input))
        (numbers (make-hash-table)))

    (loop for i in input do
          (setf (gethash i numbers) (- 2020 i)))
    (loop for i in input do
          (if (gethash (- 2020 i) numbers)
              (return-from day1a (* i (- 2020 i)))))))

(defun day1b (input)
  (let ((input (prepare-input input)))
    (loop for a in input and i from 0 do
          (loop for b in input and j from 0 do
                (loop for c in input and k from 0
                      unless (or (= i j) (= i k) (= j k))
                      do (if (= 2020 (+ a b c))
                             (return-from day1b (* a b c)))))))) 

(defun day1 ()
  (format t "01: ~A, ~A~%" (day1a "input1.txt") (day1b "input1.txt")))
