;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 9: Rope Bridge ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun neighbours (u d)
  "All vectors at distance D of U"
  ;; TODO: generalise to any dimension and distance metric
  (loop with (x y) = u
        for i from (- d) upto d
        nconc (loop for j from (- d) upto d
                    for xi = (+ x i) and yj = (+ y j)
                    unless (and (= x xi) (= y yj))
                    collect (list xi yj))))

(defun drag-tail (head tail)
  "Drag TAIL so that it is adjacent to HEAD"
  (if (< (euclidean-distance head tail) 2)
      tail
      (iter:iter (iter:for dir iter:in (neighbours '(0 0) 1))
                 (iter:for tail* = (vec+ tail dir))
                 (iter:finding tail* minimizing (manhattan-distance tail* head)))))

(defun step-rope (knots step)
  "Move the rope comprising KNOTS by DELTA"
  (let ((knots (copy-list knots)))
    (setf (first knots) (vec+ (first knots) step))
    (loop for (head tail) on knots
          for i from 1
          while tail
          do (setf (nth i knots) (drag-tail head tail))
          finally (return knots))))

(defun move-rope (rope delta)
  "Collect snapshots of ROPEs movements as it moves to vector (+ ROPE DELTA)"
  (loop with (dx dy) = delta
        with step = (list (signum dx) (signum dy))
        for knots = (step-rope rope step) then (step-rope knots step)
        collect knots  
        until (vec= (first knots) (vec+ (first rope) delta))))

(defun aoc-2022-09a (filename)
  (loop with visited = (make-hash-table :test 'equal)   
        with initial = (loop repeat 2 collect '(0 0))
        for line in (read-from-file filename)
        for (dir d) = (str:words line)
        for delta = (vec-scale
                      (cond ((string= dir "R") '( 1  0))
                            ((string= dir "L") '(-1  0))
                            ((string= dir "U") '( 0  1))
                            ((string= dir "D") '( 0 -1))
                            (t '(0 0)))
                      (parse-integer d))

        for movements = (move-rope initial delta) then (move-rope rope delta)
        for rope = (first (last movements))

        sum (loop for (head tail) in movements
                  unless (gethash tail visited)
                  do (setf (gethash tail visited) t)
                  and count t)))

(defun aoc-2022-09b (filename)
  (loop with visited = (make-hash-table :test 'equal)   
        with initial = (loop repeat 10 collect '(0 0))
        for line in (read-from-file filename)
        for (dir d) = (str:words line)
        for delta = (vec-scale
                      (cond ((string= dir "R") '( 1  0))
                            ((string= dir "L") '(-1  0))
                            ((string= dir "U") '( 0  1))
                            ((string= dir "D") '( 0 -1))
                            (t '(0 0)))
                      (parse-integer d))

        for movements = (move-rope initial delta) then (move-rope rope delta)
        for rope = (first (last movements))

        sum (loop for rope in movements
                  for tail = (first (last rope))
                  unless (gethash tail visited)
                  do (setf (gethash tail visited) t)
                  and count t)))
