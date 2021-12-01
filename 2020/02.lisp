;; ---------------------------- ;;
;; DAY 2 - PASSWORD PHILOSOPHY  ;;
;; ---------------------------- ;;

(load "util.lisp")

(ql:quickload :alexandria)
(ql:quickload :str)

(defun get-passwords (input)
  "convert parameters into list of lists ((x y character password)...)"
  (let ((input (get-file input)))
    (mapcar #'(lambda (r)
                ;; build list (x y character) password for each row
                (list
                  (parse-integer (first (str:split "-" (first r))))
                  (parse-integer (second (str:split "-" (first r))))
                  (char (second r) 0)
                  (third r)))
            ;; split input on spaces to get each field
            (mapcar #'str:words input))))

(defun valid-range (x y ch password)
  "return T if CH appears between X and Y times in PASSWORD"
  (let ((freq (count ch password)))
    (and (>= freq x) (<= freq y))))

(defun valid-position (x y ch password)
  "return T if CH appears at position X xor position Y in PASSWORD"
  (alexandria:xor (equal ch (char password (1- x)))
                  (equal ch (char password (1- y)))))

(defun day2a (input)
  (let ((passwords (get-passwords input)))
    (count T (mapcar #'(lambda (r) (apply #'valid-range r)) passwords))))

(defun day2b (input)
  (let ((passwords (get-passwords input)))
    (count T (mapcar #'(lambda (r) (apply #'valid-position r)) passwords))))

(defun day2 ()
  (format t "02: ~A, ~A~%" (day2a "input2.txt") (day2b "input2.txt")))
