
;; ----------------------
;;   Day 9: Smoke Basin
;; ----------------------

(load "util.lisp")

(defun get-minima (grid)
  "Return coordinates (x . y) of the minima of GRID"
  (let ((m (array-dimension grid 0))
        (n (array-dimension grid 1))
        minima)
    (loop for j from 0 below m
          do (loop for i from 0 below n
                   for cell = (aref grid j i)
                   if (< cell (reduce #'min 
                                      (mapcar (lambda (n) (aref grid (rest n) (first n)))
                                              (2d-neighbours grid i j :moore))))
                   do (push (cons i j) minima)))
    minima))

(defun grid->graph (grid)
  "Convert 2D GRID to adjacency list"
  (let ((m (array-dimension grid 0))
        (n (array-dimension grid 1))
        (graph (make-hash-table :test 'equal)))
    (loop for j from 0 below m
          do (loop for i from 0 below n
                   for c = (aref grid j i)
                   do (loop for neighbour in (2d-neighbours grid i j :moore)
                            for n = (aref grid (rest neighbour) (first neighbour))
                            do (if (and (> n c) (/= n 9))
                                   (push neighbour (gethash (cons i j) graph))))))
    graph))

(defun advent-09a (filename)
  (let ((input (get-number-grid filename)))

    ;; sum of 1+ minima in grid
    (reduce #'+ (mapcar (lambda (m) (1+ (aref input (rest m) (first m))))
                        (get-minima input)))))

(defun advent-09b (filename)
  (let* ((input (get-number-grid filename))
         (graph (grid->graph input)))

    (loop for root in (get-minima input)
          collect (tree-size (bfs graph root)) into basins
          finally (return (reduce #'* (subseq (sort basins #'>) 0 3))))))
