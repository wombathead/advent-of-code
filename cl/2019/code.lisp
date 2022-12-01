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

(defstruct line
  (x0 0)
  (y0 0)
  (x1 0)
  (y1 0))

(defstruct point
  (x 0) (y 0))

(defun intersects? (p q)
  " return T if line segment P intersects line segment Q "
  (let ((x1 (line-x0 p)) (y1 (line-y0 p))
		(x2 (line-x1 p)) (y2 (line-y1 p))
		(x3 (line-x0 q)) (y3 (line-y0 q))
		(x4 (line-x1 q)) (y4 (line-y1 q))
		u)
	(unless (= 0 (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))
	  (setf u (/
				(- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4)))
				(- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))))
	  (or (and (>= u 0) (<= u 1))))))

(defun intersection-point (p q)
  (let ((x1 (line-x0 p)) (y1 (line-y0 p))
		(x2 (line-x1 p)) (y2 (line-y1 p))
		(x3 (line-x0 q)) (y3 (line-y0 q))
		(x4 (line-x1 q)) (y4 (line-y1 q))
		u)
	(setf u (/
			  (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4)))
			  (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))))
	
	;; return point (x0+u(x2-x1), y0+u(y2-y1))
	(make-point :x (+ x1 (* u (- x2 x1)))
				:y (+ y1 (* u (- y2 y1))))))

(defun is-origin? (p)
  (and (= 0 (point-x p)) (= 0 (point-y p))))

(defun manhattan-distance (p q)
  (+ (abs (- (point-x p) (point-x q))) (abs (- (point-y p) (point-y q)))))

(defun compute-intersections (wires)
  (let ((grower (first wires))
		(shower (second wires))
		intersections
		(gx1 0) (gy1 0) (gx0 0) (gy0 0))
	(loop for (gdir gdist) in grower do
		  (setf gx0 gx1)
		  (setf gy0 gy1)
		  (case gdir
			(L (decf gx1 gdist))
			(R (incf gx1 gdist))
			(U (incf gy1 gdist))
			(D (decf gy1 gdist)))
		  (let ((p (make-line :x0 gx0 :y0 gy0 :x1 gx1 :y1 gy1))
				q
				(sx0 0) (sy0 0) (sx1 0) (sy1 0))
			(loop for (sdir sdist) in shower do
				  (setf sx0 sx1)
				  (setf sy0 sy1)
				  (case sdir
					(L (decf sx1 sdist))
					(R (incf sx1 sdist))
					(U (incf sy1 sdist))
					(D (decf sy1 sdist)))
				  (setf q (make-line :x0 sx0 :y0 sy0 :x1 sx1 :y1 sy1))
				  (if (intersects? p q)
					(let ((r (intersection-point p q)))
					  (unless (is-origin? r)
						(push (intersection-point p q) intersections)))))))
	intersections))

(defun shortest-manhattan (intersections)
  (apply #'min (mapcar #'(lambda (p)
						   (manhattan-distance (make-point :x 0 :y 0) p))
					   intersections)))

(defun prepare-wires (s)
  (let ((wires (mapcar #'(lambda (r)
						   (str:split "," r)) s)))
	(mapcar #'(lambda (r)
				(loop for e in r collect
					  (list
						(intern (string (char e 0)))
						(parse-integer (subseq e 1))))) wires)))

(defun test1 ()
  (let ((wires '("R75,D30,R83,U83,L12,D49,R71,U7,L72"
				 "U62,R66,U55,R34,D71,R55,D58,R83")))
	(setf wires (mapcar #'(lambda (r)
							(str:split "," r)) wires))
	(setf wires (mapcar #'(lambda (r)
							(loop for e in r collect
								  (list
									(intern (string (char e 0)))
									(parse-integer (subseq e 1))))) wires))))

(defun day3 ()
  (defun get-wires (input)
	(let ((input (get-file input)))
	  (setf input (mapcar #'(lambda (r)
							  (str:split "," r)) input))
	  (setf input (mapcar #'(lambda (r)
							  (loop for e in r collect
									(list
									  (intern (string (char e 0)))
									  (parse-integer (subseq e 1))))) input))))

  (defun day3a (input)
	(let ((wires (get-wires input))
		  grower	; grower is the one grown in each time step
		  shower	; shower is built entirely each time to check intersections
		  (gi 0)		; current x1 endpoint of grower
		  (gj 0)		; current y1 endpoint of grower
		  (gi-prev 0)	; current x0 endpoint of grower
		  (gj-prev 0)	; current y0 endpoint of grower
		  intersections)
	  (setf grower (first wires))
	  (setf shower (second wires))
	  (loop for (gdir gdist) in grower do
			(setf gi-prev gi)
			(setf gj-prev gj)
			(case gdir
			  (L (decf gi gdist))
			  (R (incf gi gdist))
			  (U (incf gj gdist))
			  (D (decf gj gdist)))
			(let ((p (make-line :x0 gi-prev :y0 gj-prev
								:x1 gi :y1 gj))
				  (si 0)
				  (sj 0)
				  (si-prev 0)
				  (sj-prev 0)
				  q)
			  (loop for (sdir sdist) in shower do
					(setf si-prev si)
					(setf sj-prev sj)
					(case sdir
					  (L (decf si sdist))
					  (R (incf si sdist))
					  (U (incf sj sdist))
					  (D (decf sj sdist)))
					(setf q (make-line :x0 si-prev :y0 sj-prev
									   :x1 si :y1 sj))
					(if (intersects? p q)
					  (push (intersection-point p q) intersections)))))

	  (mapcar #'(lambda (p)
				   (manhattan-distance (make-point :x 0 :y 0) p))
			   intersections)))

  (day3a "input3.txt"))
