;;;; utility functions

(in-package :aoc)

(defun read-from-file (filename)
  "Return contents of FILENAME as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))

(defun print-hash-table (ht)
  (maphash (lambda (k v) (format t "~A: ~A~%" k v)) ht))

(defun empty-string-p (string)
  (zerop (length string)))

(defun char-difference (a b)
  (- (char-code a) (char-code b)))

(defun chars->string (character-list)
  (coerce character-list 'string))

(defun nth-char= (string n character)
  "Return T if the Nth character of STRING equals CHARACTER"
  (char= (char string n) character)) 

(defun get-numbers (filename)
  "Parse a single line of numbers contained in FILENAME"
  (mapcar #'parse-integer (str:split "," (first (read-from-file filename)))))

(defun read-number-lists (filename)
  "Parse FILENAME as a list of lists of numbers"
  (mapcar (lambda (line)
            (map 'list (lambda (char) (digit-char-p char)) line))
          (read-from-file filename)))

(defun read-number-grid (filename)
  "Parse FILENAME as a 2D array of numbers"
  (let* ((data (read-number-lists filename))
         (m (length data))
         (n (length (first data))))
    (make-array (list m n) :initial-contents data)))

(defun matrix-row (matrix i)
  "Return row I of MATRIX as a list"
  (loop for j from 0 below (array-dimension matrix 1)
        collect (aref matrix i j)))

(defun matrix-rows (matrix)
  "Return the rows of 2D MATRIX as a list of lists"
  (loop for i from 0 below (array-dimension matrix 1)
        collect (matrix-row matrix i)))

(defun matrix-column (matrix j)
  "Return column J of MATRIX as a list"
  (loop for i from 0 below (array-dimension matrix 0)
        collect (aref matrix i j)))

(defun matrix-columns (matrix)
  "Return the columns of 2D MATRIX as a list of lists"
  (loop for j from 0 below (array-dimension matrix 0)
        collect (matrix-column matrix j)))

(defun median (list)
  "The median of LIST of numbers"
  (let ((numbers (sort (coerce list 'vector) #'<))
        (n (length list)))
    (if (oddp n)
        (aref numbers (floor n 2))
        (/ (+ (aref numbers (1- (floor n 2)))
              (aref numbers (floor n 2)))
           2))))

(defun vec+ (u v)
  (mapcar #'+ u v))

(defun vec- (u v)
  (mapcar #'- u v))

(defun vec= (u v)
  (every (lambda (ui vi) (= ui vi)) u v))

(defun vec-dot (u v)
  (reduce #'+ (mapcar #'* u v)))

(defun vec-scale (u k)
  (mapcar (lambda (ui) (* ui k)) u))

(defun pnorm (u p)
  (expt (reduce #'+ (mapcar (lambda (ui) (expt (abs ui) p)) u))
        (/ p)))

(defun normalized (u p)
  (vec-scale u (/ (pnorm u p))))

