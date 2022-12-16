;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 14: Regolith Reservoir ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun file->rock-paths (filename)
  (loop for line in (read-from-file filename)
        for matches = (ppcre:all-matches-as-strings "\\d+,\\d+" line)
        collect (loop for (x y) in (mapcar (lambda (m) (str:split "," m)) matches)
                    collect (cons (parse-integer x) (parse-integer y)))))

(defun file->cave (filename)
  (loop with cave = (make-hash-table :test 'equal)
        for path in (file->rock-paths filename)
        do (loop for (src dst) on path
                 while dst
                 for (x1 . y1) = src and (x2 . y2) = dst
                 for dx = (signum (- x2 x1)) and dy = (signum (- y2 y1))
                 do (loop for xi = x1 then (+ xi dx)
                          for yi = y1 then (+ yi dy)
                          do (setf (gethash (cons xi yi) cave) #\#)
                          until (and (= xi x2) (= yi y2))))
        finally (return cave)))

(defun print-rock-grid (ht)
  (loop with x-min = (reduce #'min (mapcar #'car (hash-table-keys ht)))
        with y-min = (reduce #'min (mapcar #'cdr (hash-table-keys ht)))
        with x-max = (reduce #'max (mapcar #'car (hash-table-keys ht)))
        with y-max = (reduce #'max (mapcar #'cdr (hash-table-keys ht)))
        for y from y-min upto y-max
        do (loop for x from x-min upto x-max
                 do (write-char (if (gethash (cons x y) ht)
                                    (gethash (cons x y) ht)
                                    #\.)))
        do (terpri)))

(defun step-sand (grid x y)
  "Return coordinates (X* . Y*) for the grain of sand at (X . Y) in GRID"
  (flet ((free-space-p (coord)
           (let ((cell (gethash coord grid)))
             (or (null cell) (char= cell #\.)))))
    (let ((down (cons x (1+ y)))
          (left (cons (1- x) (1+ y)))
          (right (cons (1+ x) (1+ y))))
      (if (free-space-p down)
        down
        (if (free-space-p left)
            left
            (if (free-space-p right)
                right
                (cons x y)))))))

(defun aoc-2022-14a (filename)
  (loop with cave = (file->cave filename)
        with lowest-wall = (reduce #'max (mapcar #'cdr (hash-table-keys cave)))
        with lowest-grain = 0
        until (> lowest-grain lowest-wall)
        sum (loop for (x . y) = '(500 . 0) then (cons x* y*)
                  for (x* . y*) = (step-sand cave x y)
                  for rested = (and (= x x*) (= y y*))
                  do (setf (gethash (cons x y) cave) #\.
                           (gethash (cons x* y*) cave) #\o
                           lowest-grain y*)
                  count rested
                  until (or (> lowest-grain lowest-wall) rested))))

(defun aoc-2022-14b (filename)
  (loop with cave = (file->cave filename)
        with lowest-wall = (reduce #'max (mapcar #'cdr (hash-table-keys cave)))
        with spout = '(500 . 0)
        until (and (gethash spout cave) (char= (gethash spout cave) #\o))
        count (loop for (x . y) = spout then (cons x* y*)
                  for (x* . y*) = (step-sand cave x y)
                  for rested = (or (= y* (1+ lowest-wall)) (and (= x x*) (= y y*)))
                  do (setf (gethash (cons x y) cave) #\.
                           (gethash (cons x* y*) cave) #\o)
                  if rested return t)))
