;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 12: Hill Climbing Algorithm ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun index-of (matrix thing &optional (test 'equal))
  (loop with (n m) = (array-dimensions matrix)
        for i from 0 below n
        do (loop for j from 0 below m
                 if (funcall test (aref matrix i j) thing)
                 do (return-from index-of (cons i j)))))

(defun make-hill-graph (filename)
  (loop with graph = (make-hash-table :test 'equal)
        with input = (read-from-file filename)
        with n = (length input) and m = (length (first input))
        with matrix = (file->matrix filename)

        for line in input
        for y from 0 below n
        do (loop for c across line
                 for x from 0 below m
                 do (loop for (dy dx) in '((-1 0) (0 1) (1 0) (0 -1))
                          for yi = (+ y dy) and xi = (+ x dx)
                          unless (or (minusp yi) (minusp xi)
                                     (> yi (1- n)) (> xi (1- m)))
                          do (let ((curr (case c
                                           (#\S #\a)
                                           (#\E #\z)
                                           (otherwise c)))
                                   (adj (case (aref matrix yi xi)
                                          (#\S #\a)
                                          (#\E #\z)
                                          (otherwise (aref matrix yi xi)))))
                               (when (< (char-difference adj curr) 2)
                                 (push (list (cons yi xi) 1)
                                       (gethash (cons y x) graph))))))

        finally (return graph)))

(defun aoc-2022-12a (filename)
  (let* ((matrix (file->matrix filename))
         (graph (make-hill-graph filename))
         (start (index-of matrix #\S))
         (end (index-of matrix #\E)))
    (path-weight (bfs graph start) end start)))

(defun possible-start-nodes (matrix char)
  (loop with (n m) = (array-dimensions matrix)
        for i from 0 below n
        nconc (loop for j from 0 below m
                    if (char= (aref matrix i j) char)
                    collect (cons i j))))

(defun aoc-2022-12b (filename)
  (flet
    ((solution-one ()
       "Iterate over all possible start points and minimize the length of the path"
       (loop with matrix = (file->matrix filename)
             with graph = (make-hill-graph filename)
             with end = (index-of matrix #\E)
             with pred = (bfs (reverse-edges graph) end)
             for start in (possible-start-nodes matrix #\a)
             for length = (path-weight pred start end)
             if length
             minimize length))

     (solution-two ()
       "Nicer solution that uses BFS that stops early"
       (loop with matrix = (file->matrix filename)
             with graph = (reverse-edges (make-hill-graph filename))
             with end = (index-of matrix #\E)
             with visited = (make-hash-table :test (hash-table-test graph)) 
             with pred = (make-hash-table :test (hash-table-test graph)) 
             with Q = (list end)
             for u = (pop Q)
             for (y . x) = u
             do (loop for e in (gethash u graph)
                      for (v weight) = e
                      unless (gethash v visited)
                      do (setf (gethash v visited) t
                               Q (append Q (list v)))
                      (push (list u weight) (gethash v pred)))
             until (char= (aref matrix y x) #\a)
             finally (return (loop for (v w) = (first (gethash u pred))
                                   then (first (gethash v pred))
                                   sum w    
                                   until (funcall (hash-table-test graph) v end))))))

    (solution-two)))
