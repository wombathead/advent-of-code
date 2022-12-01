
;; ------------------
;;   Day 15: Chiton
;; ------------------

(load "util.lisp")

(ql:quickload :alexandria)

(defun grid->graph (grid)
  (let ((G (make-hash-table :test 'equal)))
    (loop for j from 0 below (array-dimension grid 0)
          do (loop for i from 0 below (array-dimension grid 1)
                   for c = (aref grid j i)
                   do (loop for n in (2d-neighbours grid i j :moore)
                            for (x . y) = n
                            for w = (aref grid y x)
                            do (push (cons n w) (gethash (cons i j) G)))))
    G))

(defun advent-15a (filename)
  (let* ((grid (get-number-grid filename))
         (G (grid->graph grid))
         (m (array-dimension grid 0))
         (n (array-dimension grid 1))
         (start '(0 . 0))
         (target (cons (1- n) (1- m)))
         (pred (dijkstra G start target)))
    
    (loop for (u . w) = (gethash target pred) then (gethash u pred)
          for (x . y) = u
          until (equal u start)
          sum (aref grid y x) into s
          finally (return (+ s (aref grid (cdr target) (car target)))))))

(defun advent-15b (filename)
  (let* ((input (get-number-grid filename))
         (m (array-dimension input 0))
         (n (array-dimension input 1))
         (grid (make-array (list (* m 5) (* n 5))))
         (start '(0 . 0))
         (target (cons (1- (* n 5)) (1- (* m 5))))
         G
         pred)
    ;; tile the original input according to the rules
    (loop for j from 0 below (* m 5)
          do (loop for i from 0 below (* n 5)
                   for x-repeat = (floor i n)
                   for y-repeat = (floor j m)
                   for entry = (+ (aref input (mod j m) (mod i n)) x-repeat y-repeat)
                   for value = (if (> entry 9) (- entry 9) entry)
                   do (setf (aref grid j i) value)))
    ;; build the graph from the tiling
    (setf G (grid->graph grid)
          pred (dijkstra G start target))

    (loop for (u . w) = (gethash target pred) then (gethash u pred)
          for (x . y) = u
          until (equal u start)
          sum (aref grid y x) into s
          finally (return (+ s (aref grid (cdr target) (car target)))))))
