

;; -------------------------
;;   Day 11: Dumbo Octopus
;; -------------------------

(load "util.lisp")

(ql:quickload :alexandria)

(defun step-grid (grid)
  (let ((grid (alexandria:copy-array grid))
        (m (array-dimension grid 0))
        (n (array-dimension grid 1))
        (flashed (make-hash-table :test 'equal))
        (flashes 0))
    
    (loop for j from 0 below m
          do (loop for i from 0 below n
                   do (incf (aref grid j i))))

    (loop for changed? = nil
          do (loop for j from 0 below m
                   do (loop for i from 0 below n
                            for c = (aref grid j i)
                            if (and (not (gethash (cons i j) flashed)) (> c 9))
                            do (setf (gethash (cons i j) flashed) 1
                                     flashes (1+ flashes)
                                     changed? t)
                            (loop for (x . y) in (2d-neighbours grid i j :von-neumann)
                                  do (incf (aref grid y x)))))
          while changed?)
    
    (loop for j from 0 below m
          do (loop for i from 0 below n
                   if (gethash (cons i j) flashed)
                   do (setf (aref grid j i) 0)))

    (values grid flashes)))

(defun advent-11a (filename)
  (loop with grid = (get-number-grid filename) and flashes
        for i from 1 upto 100
        do (setf (values grid flashes) (step-grid grid))
        sum flashes))

(defun advent-11b (filename)
  (loop with grid = (get-number-grid filename)
        and m = (array-dimension grid 0) and n = (array-dimension grid 1)
        with flashes
        for i from 1
        do (setf (values grid flashes) (step-grid grid))
        if (= flashes (* n m))
        return i))
