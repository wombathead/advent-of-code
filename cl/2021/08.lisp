
;; -------------------------------
;;   Day 8: Seven Segment Search
;; -------------------------------

(defun advent-08a (filename)
  (let ((input (read-from-file filename)))
    (loop for line in input
          for (signal-pattern output-value) = (str:split " | " line)
          sum (loop for value in (str:words output-value)
                    for length = (length value)
                    count (member length '(2 3 4 7))))))

(defun advent-08b (filename)

  (flet ((deduce-codes (pattern)

           (let ((number-codes (make-array '(10)))
                 (code-numbers (make-hash-table :test 'equal)))

             (flet ((deduce-code (digit predicate)
                      (loop for p in pattern
                            for code = (sort p #'char<)
                            for code-chars = (coerce code 'list)
                            for digit-chars = (coerce (aref number-codes digit) 'list)
                            when (funcall predicate)
                            do (setf (gethash code code-numbers) digit
                                     (aref number-codes digit) code)))))
             
             ;; first get the numbers we know straight away
             (loop for p in pattern
                   for code = (sort p #'char<)
                   do (let ((number (case (length code)
                                      (2 1)
                                      (3 7)
                                      (4 4)
                                      (7 8))))
                        (when (and number (not (gethash code code-numbers)))
                          (setf (gethash code code-numbers) number
                                (aref number-codes number) code))))

             ;; 3s are only codes of length 5 to contain all of 1's segments
             (loop for p in pattern
                   for code = (sort p #'char<)
                   for code-chars = (coerce code 'list)
                   for one-chars = (coerce (aref number-codes 1) 'list)
                   if (and (= (length code) 5)
                           (subsetp one-chars code-chars)
                           (not (gethash code code-numbers)))
                   do (setf (gethash code code-numbers) 3
                            (aref number-codes 3) code))

             ;; 9s are the only codes of length 6 to contain all of 3's segments
             (loop for p in pattern
                   for code = (sort p #'char<)
                   for code-chars = (coerce code 'list)
                   for three-chars = (coerce (aref number-codes 3) 'list)
                   if (and (= (length code) 6)
                           (subsetp three-chars code-chars)
                           (not (gethash code code-numbers)))
                   do (setf (gethash code code-numbers) 9
                            (aref number-codes 9) code))

             ;; 0s are the only codes of length 6 containing all of 1's segments
             ;; and is not equal to 9's code
             (loop for p in pattern
                   for code = (sort p #'char<)
                   for code-chars = (coerce code 'list)
                   for one-chars = (coerce (aref number-codes 1) 'list)
                   if (and (= (length code) 6)
                           (subsetp one-chars code-chars)
                           (string/= code (aref number-codes 9))
                           (not (gethash code code-numbers)))
                   do (setf (gethash code code-numbers) 0
                            (aref number-codes 0) code))

             ;; 6s are now the only remaining codes of length 6
             (loop for p in pattern
                   for code = (sort p #'char<)
                   if (and (= (length code) 6)
                           (string/= code (aref number-codes 0))
                           (string/= code (aref number-codes 9))
                           (not (gethash code code-numbers)))
                   do (setf (gethash code code-numbers) 6
                            (aref number-codes 6) code))

             ;; 5s are the only codes of length 5 where 6's code contains all of
             ;; its segments
             (loop for p in pattern
                   for code = (sort p #'char<)
                   for code-chars = (coerce code 'list)
                   for six-chars = (coerce (aref number-codes 6) 'list)
                   if (and (= (length code) 5)
                           (subsetp code-chars six-chars)
                           (not (gethash code code-numbers)))
                   do (setf (gethash code code-numbers) 5
                            (aref number-codes 5) code))

             ;; finally, 2s are the only remaining codes of length 5
             (loop for p in pattern
                   for code = (sort p #'char<)
                   if (and (= (length code) 5)
                           (string/= code (aref number-codes 3))
                           (string/= code (aref number-codes 5))
                           (not (gethash code code-numbers)))
                   do (setf (gethash code code-numbers) 2
                            (aref number-codes 2) code))  

             code-numbers)))

    (loop with input = (read-from-file filename)
          for line in input
          for (signal-pattern output-value) = (str:split " | " line)
          for pattern = (str:words signal-pattern)
          for code-numbers = (deduce-codes pattern)
          sum (loop for c in (reverse (str:words output-value))
                    for code = (sort c #'char<)
                    for i from 0
                    for number = (gethash code code-numbers)
                    then (+ number (* (gethash code code-numbers) (expt 10 i)))
                    finally (return number)))))
