
(in-package :aoc)

(defun count-neighbours (grid x y z)
  ;; TODO: more general approach?
  (loop for dir in '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0) (0 0 1) (0 0 -1))
        for idx = (vec+ (list x y z) dir)
        count (gethash idx grid)))

(defun exposed-sides (grid x y z)
  (- 6 (count-neighbours grid x y z)))

(defun file->grid (filename)
  (loop with ht = (make-hash-table :test 'equal)
        for line in (read-from-file filename)
        for coords = (mapcar #'parse-integer (str:split "," line))
        do (setf (gethash coords ht) t)
        finally (return ht)))

(defun aoc-2022-18a (filename)
  (loop with grid = (loop with ht = (make-hash-table :test 'equal)
                          for line in (read-from-file filename)
                          for coords = (mapcar #'parse-integer (str:split "," line))
                          do (setf (gethash coords ht) t)
                          finally (return ht))
        for (x y z) in (hash-table-keys grid)
        sum (exposed-sides grid x y z)))

(defun aoc-2022-18b (filename)
  (let*
    ((coords (mapcar (lambda (l) (mapcar #'parse-integer (str:split "," l)))
                     (read-from-file filename)))

     (lava-grid (file->grid filename))

     (min-coordinate
       (mapcar (lambda (i)
                 (reduce #'min (mapcar (lambda (c) (nth i c))
                                       coords)))
               '(0 1 2)))

     (max-coordinate
       (mapcar (lambda (i)
                 (reduce #'max (mapcar (lambda (c) (nth i c))
                                       coords)))
               '(0 1 2)))

     ;; find all cells reachable from the exterior of the lava droplet
     (exterior-cells
       (loop with graph = (make-hash-table :test 'equal)
             with (xmin ymin zmin) = min-coordinate
             with (xmax ymax zmax) = max-coordinate
             for x from (1- xmin) upto (1+ xmax)
             do (loop for y from (1- ymin) upto (1+ ymax)
                      do (loop for z from (1- zmin) upto (1+ zmax)
                               for cell = (list x y z)
                               unless (gethash cell lava-grid)
                               do (loop for dir in '((1 0 0)
                                                     (-1 0 0)
                                                     (0 1 0)
                                                     (0 -1 0)
                                                     (0 0 1)
                                                     (0 0 -1))
                                        for adj = (vec+ cell dir)
                                        unless (gethash adj lava-grid)
                                        do (push (list adj 1) (gethash cell graph)))))
             finally (return graph)))

     ;; this cell is guaranteed to be on the outside
     (start-cell (mapcar #'1- min-coordinate)))

    ;; find all nodes reachable from the exterior and count the number of 
    ;; lava cells they share a face with
    (loop with exterior = (bfs exterior-cells start-cell) 
          with keys = (union (hash-table-keys exterior) (list start-cell))
          for (x y z) in keys
          sum (count-neighbours lava-grid x y z))))
