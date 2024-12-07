;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 6: Guard Gallivant ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:aoc)


(flet ((out-of-bounds-p (x y n m)
         (or (minusp x) (minusp y)
             (> x (1- n)) (> y (1- m))))


       (char-at (x y grid)
         (aref grid y x))


       (loop-present-p (x y grid)
         (iter
           (with (m n) = (array-dimensions grid))
           (with directions = '((0 -1) (1 0) (0 1) (-1 0)))
           (with idx = 0)
           (with direction = (nth 0 directions))
           (with visited = (make-hash-table :test 'equal))

           (with p = (list x y))

           (for q = (vec+ p direction))
           (for (qx qy) = q)

           (for loop? = (equal direction (gethash p visited)))

           (until (or loop? (out-of-bounds-p qx qy n m)))

           (case (char-at qx qy grid)
             (#\. (setf p q
                        (gethash p visited) direction))

             (#\# (setf direction (nth (mod (incf idx) 4) directions)))

             (#\^ (setf (aref grid qy qx) #\.)))

           (finally (return loop?))))


       (starting-position (grid)
         (iter outer
           (with (m n) = (array-dimensions grid))
           (for y from 0 to (1- m))
           (iter inner
             (for x from 0 to (1- n))
             (for char = (char-at x y grid))
             (in outer (finding (list x y) such-that (char= char #\^)))))))


  (defun aoc-2024-06a (filename)
    (iter
      (with input = (read-from-file filename))
      (with grid = (make-array (list (length input)
                                     (length (first input)))
                               :initial-contents input))
      (with (m n) = (array-dimensions grid))

      (with pos = (starting-position grid))

      (with visited = (make-hash-table :test 'equal))
      (initially (setf (gethash pos visited) t))

      (with directions = '((0 -1) (1 0) (0 1) (-1 0)))
      (with idx = 0)
      (with direction = (nth idx directions))

      (for p = (vec+ pos direction))
      (for (xi yi) = p)

      (until (out-of-bounds-p xi yi n m))

      (case (char-at xi yi grid)
        (#\. (setf pos p
                   (gethash pos visited) t))

        (#\# (setf direction (nth (mod (incf idx) 4) directions)))

        (#\^ (setf (aref grid yi xi) #\.)))

      (finally (return (length (hash-table-keys visited))))))


  (defun aoc-2024-06b (filename)
    (iter
      (with grid = (read-character-grid filename))
      (with (m n) = (array-dimensions grid))
      (with (sx sy) = (starting-position grid))

      (for (x y) in (loop for y from 0 upto (1- m) nconc (loop for x from 0 upto (1- n) collect (list x y))))
      (for copy = (alexandria:copy-array grid))
      (setf (aref copy x y) #\#)
      (counting (loop-present-p sx sy copy)))))
