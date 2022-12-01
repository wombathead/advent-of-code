;; ---------------------------- ;;
;; DAY 3 - TOBOGGAN TRAJECTORY  ;;
;; ---------------------------- ;;

(load "util.lisp")

(defun prepare-map (input)
  (let ((terrain (get-file input)))
    (make-array (list (length terrain) (length (first terrain)))
                :initial-contents terrain)))

(defun traverse (terrain dx dy)
  (let ((collisions 0)
        (i 0) (j 0)
        width height)
    (setf height (array-dimension terrain 0)
          width (array-dimension terrain 1))
    (loop while (< j height) do
          (if (>= i width)
              (setf i (mod i width)))
          (if (equal (aref terrain j i) #\#)
              (incf collisions))
          (incf i dx)
          (incf j dy))
    collisions))

(defun day3a (input)
  (let ((terrain (prepare-map input)))
    (traverse terrain 3 1)))

(defun day3b (input)
  (let ((terrain (prepare-map input)))
    ;; calculate collisions for all slopes, and multiply together
    (apply #'* (loop for (dx dy) in '((1 1) (3 1) (5 1) (7 1) (1 2))
                     collect (traverse terrain dx dy)))))

(defun day3 ()
  (format t "03: ~A, ~A~%" (day3a "input3.txt") (day3b "input3.txt")))
