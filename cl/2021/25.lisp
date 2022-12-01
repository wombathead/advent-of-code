
;; ------------------------
;;   Day 25: Sea Cucumber
;; ------------------------

(load "util.lisp")

(defun get-character-grid (filename)
  (let* ((input (get-file filename))
         (m (length input))
         (n (length (first input))))
    (make-array (list m n) :initial-contents input)))

(defun step-automaton (cucumbers)
  (let ((m (array-dimension cucumbers 0))
        (n (array-dimension cucumbers 1))
        changed?)
    (flet ((move-east-cucumbers (cucumbers)
             (let ((result (alexandria:copy-array cucumbers)))
               (loop for j from 0 below m
                     do (loop for i from 0 below n
                              for c = (aref cucumbers j i)
                              if (char= #\> c)
                              do (when (char= #\. (aref cucumbers j (mod (1+ i) n)))
                                   (setf (aref result j i) #\.
                                         (aref result j (mod (1+ i) n)) #\>
                                         changed? t))))
               result))

           (move-south-cucumbers (cucumbers)
             (let ((result (alexandria:copy-array cucumbers)))
               (loop for j from 0 below m
                     do (loop for i from 0 below n
                              for c = (aref cucumbers j i)
                              if (char= #\v c)
                              do (when (char= #\. (aref cucumbers (mod (1+ j) m) i))
                                   (setf (aref result j i) #\.
                                         (aref result (mod (1+ j) m) i) #\v
                                         changed? t))))
               result)))
      
      (values (move-south-cucumbers (move-east-cucumbers cucumbers)) changed?))))

(defun advent-25a (filename)
  (let ((input (get-character-grid filename)))
    (multiple-value-bind (grid changed?) (step-automaton input)
      (loop for i from 1 
            until (not changed?)
            do (setf (values grid changed?) (step-automaton grid))
            finally (return i)))))
