;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 4: Ceres Search ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:aoc)


(flet ((search-for-substring (substring x y grid direction)
         (destructuring-bind (m n) (array-dimensions grid)
           (cond
             ((empty-string-p substring) t)
             (t (when (and (>= x 0) (>= y 0) (< x n) (< y m))
                  (let ((c (aref grid y x))
                        (xy (vec+ (list x y) direction)))
                    (when (char= c (aref substring 0))
                      (destructuring-bind (x y) xy
                        (search-for-substring (subseq substring 1) x y grid direction))))))))))


  (defun aoc-2024-04a (filename)
    (flet ((count-occurences (word grid)
             (iter
               (with (m n) = (array-dimensions grid))
               (with cells = (alexandria:map-product
                              'list
                              (loop for x from 0 to (1- n) collect x)
                              (loop for y from 0 to (1- m) collect y)))

               (for (x y) in cells)
               (sum (iter
                      (for direction in (alexandria:map-product 'list '(-1 0 1) '(-1 0 1)))
                      (for (dx dy) = direction)
                      (unless (= dx dy 0))
                      (counting (search-for-substring word x y grid direction)))))))

      (count-occurences "XMAS" (read-character-grid filename))))


  (defun aoc-2024-04b (filename)
    (flet ((count-occurences (centres)
             "There is an X-MAS iff there are exactly two equal centres and we only go diagonally"
             (loop for point in centres
                   count (= 2 (count point centres :test 'equal))
                   do (setf centres (rest centres))))

           (string-centre (string x y direction)
             "Find the centre of the string starting at (x,y) going in direction"
             (let ((n (length string))
                   (p (list x y)))
               (vec+ p (vec-scale direction (floor n 2))))))

      (iter
        (with grid = (read-character-grid filename))
        (with (m n) = (array-dimensions grid))
        (with cells = (alexandria:map-product
                       'list
                       (loop for x from 0 to (1- n) collect x)
                       (loop for y from 0 to (1- m) collect y)))

        (for (x y) in cells)
        (nconcing (iter
                    (for direction in (alexandria:map-product 'list '(-1 1) '(-1 1)))
                    (when (search-for-substring "MAS" x y grid direction)
                      (collect (string-centre "MAS" x y direction))))
                  into centres)

        (finally (return (count-occurences centres)))))))
