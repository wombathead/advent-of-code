;;;; utility functions

(in-package :aoc)

(defun read-from-file (filename)
  "Return contents of FILENAME as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))

(defun empty-string-p (string)
  (zerop (length string)))

(defun char-difference (a b)
  (- (char-code a) (char-code b)))

(defun nth-char= (string n character)
  "Return T if the Nth character of STRING equals CHARACTER"
  (char= (char string n) character)) 

(defun get-numbers (filename)
  "Parse a single line of numbers contained in FILENAME"
  (mapcar #'parse-integer (str:split "," (first (read-from-file filename)))))

(defun get-number-lists (filename)
  "Parse FILENAME as a list of lists of numbers"
  (mapcar (lambda (line)
            (map 'list (lambda (char) (digit-char-p char)) line))
          (read-from-file filename)))

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

(defun median (list)
  "The median of LIST of numbers"
  (let ((numbers (sort (coerce list 'vector) #'<))
        (n (length list)))
    (if (oddp n)
        (aref numbers (floor n 2))
        (/ (+ (aref numbers (1- (floor n 2)))
              (aref numbers (floor n 2)))
           2))))

