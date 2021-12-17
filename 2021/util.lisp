;;;; util.lisp

(ql:quickload :str)

(defun get-file (filename)
  "Return contents of FILENAME as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))

(defun get-numbers (filename)
  "Parse a single line of numbers contained in FILENAME"
  (mapcar #'parse-integer (str:split "," (first (get-file filename)))))

(defun get-number-lists (filename)
  "Parse FILENAME as a list of lists of numbers"
  (mapcar (lambda (line)
            (map 'list (lambda (char) (digit-char-p char)) line))
          (get-file filename)))

(defun get-number-grid (filename)
  "Parse FILENAME as a 2D array of numbers"
  (let* ((data (get-number-lists filename))
         (m (length data))
         (n (length (first data))))
    (make-array (list m n) :initial-contents data)))

(defun matrix-rows (matrix)
  "Return the rows of 2D MATRIX as a list of lists"
  (loop with (n m) = (array-dimensions matrix)
        for j from 0 below n
        collect (loop for i from 0 below m
                      collect (aref matrix j i))))

(defun matrix-columns (matrix)
  "Return the columns of 2D MATRIX as a list of lists"
  (loop with (n m) = (array-dimensions matrix)
        for j from 0 below n
        collect (loop for i from 0 below m
                      collect (aref matrix i j))))

(defun nth-char= (string n character)
  "Return T if the Nth character of STRING equals CHARACTER"
  (char= (char string n) character)) 

(defun median (list)
  "The median of LIST of numbers"
  (let ((numbers (sort (coerce list 'vector) #'<))
        (n (length list)))
    (if (oddp n)
        (aref numbers (floor n 2))
        (/ (+ (aref numbers (1- (floor n 2)))
              (aref numbers (floor n 2)))
           2))))

(defun bfs (G n)
  "Breadth First Search starting from N"
  (let ((explored (make-hash-table :test (hash-table-test G)))
        (tree (make-hash-table :test (hash-table-test G)))
        (Q (list n)))
    (setf (gethash n explored) t)
    (loop while Q
          for u = (pop Q)
          do (loop for v in (gethash u G)
                   unless (gethash v explored)
                   do
                   (setf (gethash v explored) t
                         Q (append Q (list v)))
                   (push v (gethash u tree)))
          finally (return tree))))

(defun dfs (G n)
  "Depth First Search starting from N"
  (let ((explored (make-hash-table :test (hash-table-test G)))
        (tree (make-hash-table :test (hash-table-test G)))
        (S (list n)))
    (setf (gethash n explored) t)
    (loop while S
          for u = (pop S)
          do (loop for v in (gethash u G)
                   unless (gethash v explored)
                   do
                   (setf (gethash v explored) t
                         S (cons v S))
                   (push v (gethash u tree)))
          finally (return tree))))

(defun tree-size (G)
  "Number of nodes in tree G"
  (loop for n being each hash-key of G
        sum (length (gethash n G)) into s
        finally (return (1+ s))))

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

(defun print-hash-table (ht)
  (maphash (lambda (k v) (format t "~A: ~A~%" k v)) ht))

(defun print-grid (grid)
  (loop for j from 0 below (array-dimension grid 0)
        do (loop for i from 0 below (array-dimension grid 1)
                 do (format t "~A" (aref grid j i)))
        (terpri)))

