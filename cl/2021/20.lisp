
(load "util.lisp")

(defun point-neighbours (x y)
  (loop for j from -1 upto 1
        nconc (loop for i from -1 upto 1
                    for xi = (+ x i) and yj = (+ y j)
                    collect (cons xi yj))))

(defun enhance-pixel (pixel image algorithm)
  "Return the character PIXEL in IMAGE turns into as a result of applying the algorithm"
  (let ((index (loop for n in (reverse (point-neighbours (car pixel) (cdr pixel)))
                     for i from 0 
                     if (gethash n image)
                     sum (expt 2 i))))
    (char algorithm index)))

(defun minima-maxima (ht)
  (loop for (x . y) in (alexandria:hash-table-keys ht)
        minimize x into a
        maximize x into b
        minimize y into c
        maximize y into d
        finally (return (values a b c d))))

#|
(loop with enhanced-image = (make-hash-table :test 'equal)
      for (x . y) in (alexandria:hash-table-keys image)
      do (loop for n in (point-neighbours x y)
               if (char= #\# (enhance-pixel n image algorithm))
               do (setf (gethash n enhanced-image) t))
      finally (return enhanced-image))
|#

(defun enhance-image (image algorithm)
  (multiple-value-bind (min-x max-x min-y max-y) (minima-maxima image)
    (loop with enhanced-image = (make-hash-table :test 'equal)
          for x from (- min-x 4) upto (+ max-x 4)
          do (loop for y from (- min-y 4) upto (+ max-y 4)
                   for p = (cons x y)
                   if (char= #\# (enhance-pixel p image algorithm))
                   do (setf (gethash p enhanced-image) t))
          finally (return enhanced-image))))

(defun print-infinite-grid (ht)
  (multiple-value-bind (min-x max-x min-y max-y) (minima-maxima ht)
    (loop for y from min-y upto max-y
          do (loop for x from min-x upto max-x
                   do (format t "~A" (if (gethash (cons x y) ht) #\# #\.))
                   finally (terpri)))))

(defun pixel-lit-p (x y image)
  (gethash (cons x y) image))

(defun count-lit-cells (image)
  (loop for (x . y) in (alexandria:hash-table-keys image)
        count (pixel-lit-p x y image)))

(defun advent-20a (filename)
  (let* ((input (get-file filename))
         (algorithm (first input))
         (input-image (loop with ht = (make-hash-table :test 'equal)
                            for row in (cddr input) and j from 0
                            do (loop for c across row and i from 0
                                     if (char= #\# c)
                                     do (setf (gethash (cons i j) ht) t))
                            finally (return ht))))

    (loop with initial = (count-lit-cells input-image)
          repeat 3
          for image = input-image then (enhance-image image algorithm) 
          do (print-infinite-grid image) (terpri)
          finally (return (count-lit-cells image)))))
