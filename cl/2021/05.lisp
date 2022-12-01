

;; -------------------------------
;;   Day 5: Hydrothermal Venture
;; -------------------------------

(ql:quickload :cl-ppcre)

(load "util.lisp")

(defun parse-line (line)
  (let (str points)
    (setf (values str points) (ppcre:scan-to-strings
                                "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"
                                line))
    (map 'vector #'parse-integer points)))

(defun advent-05a (filename)
  (let* ((vents (mapcar #'parse-line (get-file filename)))
         (m (1+ (reduce #'max (mapcar (lambda (v) (max (aref v 1) (aref v 3))) vents))))
         (n (1+ (reduce #'max (mapcar (lambda (v) (max (aref v 0) (aref v 2))) vents))))
         (grid (make-array (list m n))))

    (loop for v in vents
          for a.x = (aref v 0) and a.y = (aref v 1)
          for b.x = (aref v 2) and b.y = (aref v 3)
          for dy = (- b.y a.y) and dx = (- b.x a.x)
          if (or (zerop dy) (zerop dx))
          do (loop for j = a.y then (+ j (signum dy))
                   for i = a.x then (+ i (signum dx))
                   do (incf (aref grid j i))
                   until (and (= j b.y) (= i b.x))))

    (loop for j from 0 below m
          sum (loop for i from 0 below n 
                    count (>= (aref grid j i) 2)))))

(defun advent-05b (filename)
  (let* ((vents (mapcar #'parse-line (get-file filename)))
         (m (1+ (reduce #'max (mapcar (lambda (v) (max (aref v 1) (aref v 3))) vents))))
         (n (1+ (reduce #'max (mapcar (lambda (v) (max (aref v 0) (aref v 2))) vents))))
         (grid (make-array (list m n))))

    (loop for v in vents
          for a.x = (aref v 0) and a.y = (aref v 1)
          for b.x = (aref v 2) and b.y = (aref v 3)
          for dy = (- b.y a.y) and dx = (- b.x a.x)
          do (loop for j = a.y then (+ j (signum dy))
                   for i = a.x then (+ i (signum dx))
                   do (incf (aref grid j i))
                   until (and (= j b.y) (= i b.x))))

    (loop for j from 0 below m
          sum (loop for i from 0 below n 
                    count (>= (aref grid j i) 2)))))
