
;; ------------------------
;;   Day 25: Sea Cucumber
;; ------------------------

(load "util.lisp")

(defun get-character-grid (filename)
  (let* ((input (get-file filename))
         (m (length input))
         (n (length (first input))))
    (make-array (list m n) :initial-contents input)))

(defun step-automaton (grid)
  (let ((m (array-dimension grid 0))
        (n (array-dimension grid 1))
        (interim (alexandria:copy-array grid))
        next-generation
        changed?)
    ;; first move all east-moving cucumbers
    (loop for j from 0 below m
          do (loop for i from 0 below n
                   for c = (aref grid j i)
                   if (char= #\> c)
                   do (when (char= #\. (aref grid j (mod (1+ i) n)))
                        (setf (aref interim j i) #\.
                              (aref interim j (mod (1+ i) n)) #\>
                              changed? t))))

    (setf next-generation (alexandria:copy-array interim))

    ;; now move all south-moving cucumbers
    (loop for j from 0 below m
          do (loop for i from 0 below n
                   for c = (aref interim j i)
                   if (char= #\v c)
                   do (when (char= #\. (aref interim (mod (1+ j) m) i))
                        (setf (aref next-generation j i) #\.
                              (aref next-generation (mod (1+ j) m) i) #\v
                              changed? t))))

    (values next-generation changed?)))

(defun advent-25a (filename)
  (let ((input (get-character-grid filename)))
    (multiple-value-bind (grid changed?) (step-automaton input)
      (loop for i from 1 
            until (not changed?)
            do (setf (values grid changed?) (step-automaton grid))
            finally (return i)))))
