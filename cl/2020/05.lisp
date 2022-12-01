;; ---------------------------- ;;
;;   DAY 5 - BINARY BOARDING    ;;
;; ---------------------------- ;;

(load "util.lisp")

(defun seat-id (boarding-pass)
  "simply interpret PASS as a binary string of the seat ID: F,L=0; B,R=1"
  (let ((str boarding-pass))
    (setf str (cl-ppcre:regex-replace-all "F" str "0"))
    (setf str (cl-ppcre:regex-replace-all "B" str "1"))
    (setf str (cl-ppcre:regex-replace-all "L" str "0"))
    (setf str (cl-ppcre:regex-replace-all "R" str "1"))
    (parse-integer str :radix 2)))

(defun day5a (input)
  (let ((passes (get-file input)))
    (apply #'max (mapcar #'seat-id passes))))

(defun day5b (input)
  (let ((ids (sort (mapcar #'seat-id (get-file input)) #'<)))
    ;; if two IDs differ by 2 then I am between them
    (loop for (a b) on ids do
          (if (= (- b a) 2)
              (return-from day5b (1+ a))))))

(defun day5 ()
  (format t "05: ~A, ~A~%" (day5a "input5.txt") (day5b "input5.txt")))


