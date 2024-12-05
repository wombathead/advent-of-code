;;;; utility functions

(in-package :aoc)

;;; reading files

(defun read-from-file (filename)
  "Return contents of FILENAME as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))


(defun read-character-grid (filename)
  (let* ((lines (read-from-file filename))
         (m (length lines))
         (n (length (first lines))))
    (make-array (list m n) :initial-contents lines)))


(defun file->matrix (filename)
  "Read FILENAME as a matrix of characters"
  (let ((input (read-from-file filename)))
    (make-array 
      (list (length input) (length (first input)))
      :initial-contents (mapcar (lambda (line) (coerce line 'list))
                                (read-from-file filename)))))

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

(defun print-hash-table (ht)
  (maphash (lambda (k v) (format t "~A: ~A~%" k v)) ht))


;;; sequences

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))


(defun all-combinations (k list)
  (let (combinations)
    (alexandria:map-combinations (lambda (c) (push c combinations)) list :length k)
    combinations))


(defun list-product (list &rest more-lists)
  (alexandria:map-product 'list list more-lists))

(defun flatten (lists)
  "Flatten lists by one level, TODO: more levels"
  (reduce #'nconc lists))

(defun slice (column lists)
  (mapcar (lambda (list) (nth column list)) lists))


(defun monotone-increasing-p (sequence &key (strict nil))
  (let ((test (if strict #'< #'<=)))
    (if (second sequence)
        (and (funcall test (first sequence) (second sequence))
             (monotone-increasing-p (rest sequence)))
        t)))

(defun monotone-decreasing-p (sequence &key (strict nil))
  (monotone-increasing-p (reverse sequence) :strict strict))

(defun monotonep (sequence &key (strict nil))
  (or (monotone-increasing-p sequence :strict strict)
      (monotone-decreasing-p sequence :strict strict)))

(defun remove-nth (n list)
  (loop for item in list
        for i from 0
        unless (= i n) collect item))


(defun replace-nth (n item list)
  (loop for e in list
        for i from 0
        collect (if (= i n) item e)))


;;; orderings

(defun define-ordering> (elements)
  "Define a total order ELEMENTS = '(A B C D ...) = A > B > C > D > ..."
  (lambda (x y) (< (position x elements) (position y elements))))


(defun define-ordering< (elements)
  (define-ordering> (reverse elements)))


(defun ordering> (x y ordering)
  (funcall ordering x y))


(defun ordering< (x y ordering)
  (ordering> y x ordering))


(defun ordering<= (x y ordering)
  (not (ordering> x y ordering)))


(defun ordering>= (x y ordering)
  (not (ordering< x y ordering)))


;;; strings 

(defun multiple-regex-replace (string regexes replacements)
  (loop for regex in regexes
        for replacement in replacements
        for s = (ppcre:regex-replace-all regex string replacement)
        then (ppcre:regex-replace-all regex s replacement)
        finally (return s)))

(defun empty-string-p (string)
  (zerop (length string)))

(defun char-difference (a b)
  (- (char-code a) (char-code b)))

(defun chars->string (character-list)
  (coerce character-list 'string))

(defun nth-char= (string n character)
  "Return T if the Nth character of STRING equals CHARACTER"
  (char= (char string n) character)) 

;;; matrices

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

(defun sorted (sequence predicate)
  (let ((seq (copy-seq sequence)))
    (sort seq predicate)))

;;; linear algebra

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

(defun manhattan-distance (u v)
  (pnorm (vec- u v) 1))

(defun euclidean-distance (u v)
  (pnorm (vec- u v) 2))

(defun normalized (u p)
  (vec-scale u (/ (pnorm u p))))

;;; graphs

(defun bfs (graph start)
  (loop with pred = (make-hash-table :test (hash-table-test graph))
        with visited = (make-hash-table :test (hash-table-test graph))
        with Q = (list start)
        initially (setf (gethash start visited) t)
        for u = (pop Q) while u
        do (loop for e in (gethash u graph) 
                 for (v weight) = e
                 unless (gethash v visited)
                 do (progn
                      (setf (gethash v visited) t
                            Q (append Q (list v)))
                      (push (list u weight) (gethash v pred))))
        finally (return pred)))

(defun s-t-path (pred start end)
  "Return a path from START to END in predecessor hashtable PRED"
  (loop for u = start then v
        ;; TODO: assumes there is exactly one predecessor to U. Change/keep?
        for (v w) = (first (gethash u pred))
        if (null u) return nil
        else collect u
        until (funcall (hash-table-test pred) u end)))

(defun path-weight (pred start end)
  "Find total weight of path from START to END in predecessor hashtable PRED"
  (loop for u = start then v
        for (v w) = (first (gethash u pred))
        if (null v) return nil     ;; TODO: is this good?
        else sum w   
        until (funcall (hash-table-test pred) v end)))

(defun reverse-edges (graph)
  "Reverse all edges in "
  (loop with g = (make-hash-table :test (hash-table-test graph))
        for u in (hash-table-keys graph)
        do (loop for (v w) in (gethash u graph)
                 do (push (list u w) (gethash v g)))
        finally (return g)))

(defun total-edges (g)
  (loop for u in (hash-table-keys g)
        sum (length (gethash u g))))

(defun topological-sort (dag)
  (loop with out-degree = (loop with ht = (make-hash-table :test (hash-table-test dag))
                                for u in (hash-table-keys dag)
                                do (setf (gethash u ht) (length (gethash u dag)))
                                finally (return ht))
        
        for sink = (loop for u in (hash-table-keys out-degree)
                         if (zerop (gethash u out-degree))
                         do (decf (gethash u out-degree))
                         and return u)

        do (loop for u in (hash-table-keys out-degree)
                 do (loop for v in (gethash u dag)
                          if (funcall (hash-table-test dag) v sink)
                          do (decf (gethash u out-degree))))
        while sink
        collect sink into ordering
        finally (return (reverse ordering))))

(defun children (graph parent)
  (gethash parent graph))


(defun graph-node-restriction (ht nodes)
  ;; TODO: deal with weighted edges
  (let ((graph (make-hash-table :test (hash-table-test ht))))
    (loop for u in (hash-table-keys ht)
          if (member u nodes)
            do (setf (gethash u graph) (intersection nodes (gethash u ht))))
    graph))
