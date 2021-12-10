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

(defun tree-size (G)
  "Number of nodes in tree G"
  (loop for n being each hash-key of G
        sum (length (gethash n G)) into s
        finally (return (1+ s))))

