(ql:quickload :str)

(defun get-file (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream NIL)
		  while line
		  collect line)))

(defun day1 ()
  (defun prepare-input (input)
	(sort (remove-duplicates (mapcar #'parse-integer (get-file input))) #'<))

  (defun day1a (input)
	" find the X and Y from input such that their sum is 2020 and return X*Y "
	(let ((input (prepare-input input))
		  x y)

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
	(let ((input (prepare-input input))
		  x y z)
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

  (format T "~A~%" (day1a "input1.txt"))
  (format T "~A~%" (day1b "input1.txt")))

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

(defun day3 ()
  (defun prepare-map (input)
	(let ((terrain (get-file input)))
	  (make-array (list (length terrain) (length (first terrain)))
				  :initial-contents terrain)))

  (defun traverse (terrain dx dy)
	(let ((collisions 0)
		  (i 0) (j 0)
		  width height)
	  (setf height (array-dimension terrain 0))
	  (setf width (array-dimension terrain 1))
	  (loop while (< j height) do
			(if (>= i width)
			  (setf i (mod i width)))
			(if (equal (aref terrain j i) #\#)
			  (incf collisions))
			(incf i dx)
			(incf j dy))
	  collisions))

  (defun day3a (input)
	(let ((terrain (prepare-map input))
		  collisions)
	  (setf collisions (loop for (dx dy) in '((1 1) (3 1) (5 1) (7 1) (1 2))
							 collect (traverse terrain dx dy)))
	  (apply #'* collisions)))

  (day3a "input3.txt"))
