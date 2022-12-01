;; ---------------------------- ;;
;;    DAY 6 - CUSTOM CUSTOMS    ;;
;; ---------------------------- ;;

(load "util.lisp")

(defun prepare-answers (input)
  (let ((file (get-file input))
        (group-str "")
        (group-size 0)
        answers)
    (loop for line in file do
          (if (equal line "")
              (progn (push (list group-size group-str) answers)
                     (setf group-size 0
                           group-str ""))
              (progn (incf group-size)
                     (setf group-str
                           (concatenate 'string group-str line)))))
    answers))

(defun day6a (input)
  (let ((answers (prepare-answers input)))
    (apply #'+ (mapcar
                 ;; count the number of distinct characters 
                 #'(lambda (a)
                     (length (remove-duplicates (second a))))
                 answers))))

(defun day6b (input)
  (let ((answers (prepare-answers input)))
    (apply #'+ (mapcar
                 ;; count the number of times the frequency of a letter
                 ;; equals the group size
                 #'(lambda (a)
                     (let ((group-size (first a))
                           (answers (second a))
                           (sum 0))
                       (loop for question across (remove-duplicates answers) do
                             (if (= (count question answers) group-size)
                                 (incf sum)))
                       sum))
                 answers))))

(defun day6 ()
  (format t "06: ~A, ~A~%" (day6a "input6.txt") (day6b "input6.txt")))
