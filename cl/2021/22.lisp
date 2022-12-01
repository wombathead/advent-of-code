(load "util.lisp")

(defun advent-22a (filename)
  (flet ((ranges->xyz (ranges)
           (mapcar (lambda (r)
                     (let ((low-high (ppcre:all-matches-as-strings "-?\\d+" r)))
                       (cons (parse-integer (first low-high))
                             (parse-integer (second low-high)))))
                   ranges)))

  (loop with grid = (make-hash-table :test 'equal)
        for line in (get-file filename)
        for state = (char= #\n (char line 1))
        for ranges = (ppcre:all-matches-as-strings "-?\\d+\.\.-?\\d+" line)
        for (xrange yrange zrange) = (ranges->xyz ranges)
        for (xlo . xhi) = xrange
        for (ylo . yhi) = yrange
        for (zlo . zhi) = zrange
        unless (or
                 (< xlo -50) (> xhi 50)
                 (< ylo -50) (> yhi 50)
                 (< zlo -50) (> zhi 50))
        do (loop for x from xlo upto xhi
             do (loop for y from ylo upto yhi
                      do (loop for z from zlo upto zhi
                               do (setf (gethash (list x y z) grid) state))))
        finally (return (loop for p in (alexandria:hash-table-keys grid)
                              count (gethash p grid))))))

(defun advent-22b (filename)
  )
