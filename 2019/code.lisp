(ql:quickload :str)

(defun get-file (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream NIL)
		  while line
		  collect line)))

(defun day1 ()
  (defun calculate-fuel (m)
	(- (floor (/ m 3)) 2))

  (defun calculate-fuels (masses)
	(mapcar #'calculate-fuel masses))

  (defun day1a (input)
	(let ((input (get-file input)))
	  (setf input (mapcar #'parse-integer input))
	  (reduce #'+ (calculate-fuels input))))

  (defun day1b (input)
	(let ((input (get-file input))
		  (total 0))
	  (setf input (mapcar #'parse-integer input))
	  (loop while (not (equal input NIL)) do
			(let ((m (pop input))
				  fuel)
			  (setf fuel (calculate-fuel m))
			  (unless (<= fuel 0)
				(incf total fuel)
				(push fuel input))))
	  total))

  (format T "1a: ~D~%" (day1a "input1.txt"))
  (format T "1b: ~D~%" (day1b "input1.txt")))

(defmacro op-fun (op)
  `(cond ((= ,op 1) '+)
		 ((= ,op 2) '*)
		 ((= ,op 99) 'HALT)))

(defun day2 ()
  (defun prepare-program (input)
	(let ((program (get-file input)))
	  (setf program (mapcar #'parse-integer (str:split "," (first program))))))

  (defun execute-program (program noun verb)
	(let ((tape (copy-list program))	; output tape
		  (ip 0))						; instruction pointer
	  (setf (second tape) noun)
	  (setf (third tape) verb)
	  (loop while T do
			(let ((op (nth ip tape))
				  (a (nth (+ ip 1) tape))
				  (b (nth (+ ip 2) tape))
				  (c (nth (+ ip 3) tape)))
			  (if (equal (op-fun op) 'HALT)
				(return))
			  (setf (nth c tape)
					(funcall (op-fun op) (nth a tape) (nth b tape)))
			  (incf ip 4)))
	  tape))

  (defun day2a (input)
	(let ((program (prepare-program input)))
	  (execute-program program 12 2)))

  (defun day2b (input)
	(let ((program (prepare-program input))
		  output)
	  (block nested
			 (loop for n from 0 to 99 do
				   (loop for v from 0 to 99 do
						 (setf output (execute-program program n v))
						 (when (= (first output) 19690720)
						   (return-from nested (+ (* 100 n) v))))))))

  (day2a "input2.txt")
  (day2b "input2.txt"))
