

;; --------------------------
;;   Day 10: Syntax Scoring
;; --------------------------

(load "util.lisp")

(defun invalid-p (line)
  (loop for c across line
        with stack and invalid
        until invalid
        if (member c '(#\( #\[ #\{ #\<))
        do (push c stack)
        else
        do 
        (setf invalid (case c
                        (#\) (not (char= (pop stack) #\()))
                        (#\] (not (char= (pop stack) #\[)))
                        (#\} (not (char= (pop stack) #\{)))
                        (#\> (not (char= (pop stack) #\<)))))
        if invalid return c))

(defun advent-10a (filename)
  (let ((input (get-file filename)))
    (loop for line in input
          for c = (invalid-p line)
          with sum = 0
          if c
          do (incf sum (cond 
                         ((char= #\) c) 3)
                         ((char= #\] c) 57)
                         ((char= #\} c) 1197)
                         ((char= #\> c) 25137)
                         (t 0)))
          finally (return sum))))

(defun advent-10b (filename)
  (flet ((stack-score (stack)
           (loop for s in stack
                 with score = 0
                 do (setf score (+ (* score 5)
                                   (cond
                                     ((char= s #\() 1)
                                     ((char= s #\[) 2)
                                     ((char= s #\{) 3)
                                     ((char= s #\<) 4)
                                     (t 0))))
                 finally (return score))))

    (let* ((input (remove-if #'invalid-p (get-file filename)))
           (n (length input))
           (scores 
             (loop for line in input
                   for stack = (loop for c across line
                                     with stack
                                     do (if (member c '(#\( #\[ #\{ #\<))
                                            (push c stack)
                                            (pop stack))
                                     finally (return stack))
                   collect (stack-score stack))))
      (nth (floor n 2) (sort scores #'>)))))
