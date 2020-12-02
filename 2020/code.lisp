(ql:quickload :str)

(defun get-file (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream NIL)
		  while line
		  collect line)))

(defun day1a (input)
  " find the X and Y from input such that their sum is 2020 and return X*Y "
  (let ((input (get-file input))
		x y)
	(setf input (remove-duplicates (mapcar #'parse-integer input)))

	;; TODO: great candidate to macroise!!!
	(loop for a in input and i from 0 do
		  (loop for b in input and j from 0 do
				(unless (= i j)
				  (if (= 2020 (+ a b))
					(progn
					  (setf x a)
					  (setf y b))))))
	(* x y)))

(defun day1b (input)
  (let ((input (get-file input))
		x y z)
	(setf input (remove-duplicates (mapcar #'parse-integer input)))
	(loop for a in input and i from 0 do
		  (loop for b in input and j from 0 do
				(loop for c in input and k from 0 do
					  (unless (or (= a b) (= a c) (= b c))
						(if (= 2020 (+ a b c))
						  (progn
							(setf x a)
							(setf y b)
							(setf z c)))))))
	(* x y z)))

(defun day2a (input)
  " return the number of valid passwords from INPUT "
  (let ((input (get-file input))
		(valid 0))	;; number of invalid passwords
	(setf input (mapcar #'str:words input))
	(loop for r in input do
		  (let ((lo (parse-integer (first (str:split "-" (first r)))))
				(hi (parse-integer (second (str:split "-" (first r)))))
				(c (char (second r) 0))
				(password (third r))
				char-frequency)
			(setf char-frequency (count c password))
			(if (and (>= char-frequency lo)
					 (<= char-frequency hi))
			  (incf valid))))
	valid))

(defun day2b (input)
  (let ((input (get-file input))
		(valid 0))	;; number of invalid passwords
	(setf input (mapcar #'str:words input))
	(loop for r in input do
		  (let ((lo (parse-integer (first (str:split "-" (first r)))))
				(hi (parse-integer (second (str:split "-" (first r)))))
				(c (char (second r) 0))
				(password (third r)))
			(if (and
				  (or (equal (char password (1- lo)) c)
					  (equal (char password (1- hi)) c))
				  (not (and
						 (equal (char password (1- lo)) c)
						 (equal (char password (1- hi)) c))))
			  (incf valid))))
	valid))
