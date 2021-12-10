
;; ----------------------
;;   Day 9: Smoke Basin
;; ----------------------

(load "util.lisp")

(defun 2d-neighbours (grid x y neighbourhood-type)
  "Return coordinates (i . j) of all neighbours to (X,Y) in GRID"
  (let ((m (array-dimension grid 0))
        (n (array-dimension grid 1))
        neighbours)

    (flet ((neigbourhood-test (x y i j n m)
             (let ((xi (+ x i))
                   (yj (+ y j)))
               (case neighbourhood-type
                 (:von-neumann (or (minusp xi) (minusp yj)
                                   (>= xi n) (>= yj m)
                                   (and (= xi x) (= yj y))))
                 (:moore (or (minusp xi) (minusp yj)
                             (>= xi n) (>= yj m)
                             (and (= xi x) (= yj y))
                             (= (abs i) (abs j))))))))
      
      (loop for j from -1 upto 1
            do (loop for i from -1 upto 1
                 for xi = (+ x i) and yj = (+ y j)
                 unless (neigbourhood-test x y i j n m)
                 do (push (cons xi yj) neighbours)))
      neighbours)))

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
  (let* ((input (mapcar (lambda (line)
                          (map 'list (lambda (char) (digit-char-p char)) line))
                        (get-file filename)))
         (m (length input))
         (n (length (first input)))
         (grid (make-array (list m n) :initial-contents input)))

    ;; sum of 1+ minima in grid
    (reduce #'+ (mapcar (lambda (m) (1+ (aref grid (rest m) (first m))))
                        (get-minima grid)))))

(defun advent-09b (filename)
  (let* ((input (mapcar (lambda (line)
                          (map 'list (lambda (char) (digit-char-p char)) line))
                        (get-file filename)))
         (m (length input))
         (n (length (first input)))
         (grid (make-array (list m n) :initial-contents input))
         (graph (grid->graph grid)))

    (loop for root in (get-minima grid)
          collect (tree-size (bfs graph root)) into basins
          finally (return (reduce #'* (subseq (sort basins #'>) 0 3))))))
