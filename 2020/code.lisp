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

(defun day2 ()
  (defun get-passwords (input)
	" convert parameters into list of lists ((x y character password)...) "
	(let ((input (get-file input)))
	  (mapcar #'(lambda (r)
				  ;; build list (x y character password for each row
				  (list
					(parse-integer (first (str:split "-" (first r))))
					(parse-integer (second (str:split "-" (first r))))
					(char (second r) 0)
					(third r)))
			  ;; split input on spaces to get each field
			  (mapcar #'str:words input))))

  (defun valid-range (x y ch password)
	" return T if CH appears between X and Y times in PASSWORD "
	(let ((freq (count ch password)))
	  (and (>= freq x) (<= freq y))))

  (defun valid-position (x y ch password)
	" return T if CH appears at position X xor position Y in PASSWORD "
	(and
	  (or (equal ch (char password (1- x)))
		  (equal ch (char password (1- y))))
	  (not (and (equal ch (char password (1- x)))
				(equal ch (char password (1- y)))))))

  (defun day2a (input)
	(let ((input (get-passwords input)))
	  (count T (mapcar #'(lambda (r) (apply #'valid-range r)) input))))

  (defun day2b (input)
	(let ((input (get-passwords input)))
	  (count T (mapcar #'(lambda (r) (apply #'valid-position r)) input))))

  (format T "~A~%" (day2a "input2.txt"))
  (format T "~A~%" (day2b "input2.txt")))

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
	(let ((terrain (prepare-map input)))
	  (traverse terrain 3 1)))

  (defun day3b (input)
	(let ((terrain (prepare-map input)))
	  ;; calculate collisions for all slopes, and multiply together
	  (apply #'* (loop for (dx dy) in '((1 1) (3 1) (5 1) (7 1) (1 2))
					   collect (traverse terrain dx dy)))))

  (format T "~A~%" (day3a "input3.txt"))
  (format T "~A~%" (day3b "input3.txt")))
