
;; ----------------------
;;   Day 6: Lanternfish 
;; ----------------------

(ql:quickload :str)

(load "util.lisp")

(defun advent-06a (filename)
  (flet ((step-population (population)
           (loop for f in population
                 if (zerop f)
                 collect 6 into g
                 and collect 8 into h
                 else
                 collect (1- f) into g
                 finally (return (append g h)))))

    (let ((fish (mapcar #'parse-integer (str:split "," (first (get-file filename))))))
      (loop for i from 1 upto 80
        for f = (step-population fish) then (step-population f)
        finally (return (length f))))))

(defun advent-06b (filename)
  (flet ((step-population (population)
           (loop with n = (length population)
                 with p = (make-array (list n))
                 with new-additions = (aref population 0)
                 for i from 0 below (1- n)
                 do (setf (aref p i) (aref population (1+ i)))
                 finally
                 (incf (aref p 6) new-additions)
                 (incf (aref p 8) new-additions)
                 (return p))))

    (let ((fish (mapcar #'parse-integer (str:split "," (first (get-file filename)))))
          (population (make-array '(9))))
      (loop for i from 0 upto 8
        do (setf (aref population i) (count i fish)))
      (loop for i from 0 upto 256
        for f = population then (step-population f)
        finally (return (reduce #'+ f))))))
