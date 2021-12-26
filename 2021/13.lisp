
;; -------------------------------
;;   Day 13: Transparent Origami
;; -------------------------------

(load "util.lisp")

(defun parse-input (filename)
  (let ((input (get-file filename))
        points folds)

    (setf points (loop for line = (first input)
                       until (string= line "")
                       for (x y) = (mapcar #'parse-integer (str:split "," line))
                       do (setf input (rest input))
                       collect (cons x y)
                       finally (setf input (rest input))))

    (setf folds (loop for line in input
                      for matches = (nth-value 1 (cl-ppcre:scan-to-strings
                                                   "(x|y)=([0-9]+)"
                                                   line))
                      for axis = (read-from-string (aref matches 0))
                      for fold-line = (parse-integer (aref matches 1))
                      collect (cons axis fold-line)))

    (values points folds)))

(defun print-grid (grid)
  (loop for j from 0 below (array-dimension grid 0)
        do (loop for i from 0 below (array-dimension grid 1)
              do (format t "~A" (if (zerop (aref grid j i)) "." "#")))
        do (format t "~%")))

(defun advent-13a (filename)
  (multiple-value-bind (points folds) (parse-input filename)
    (let* ((axis (car (first folds)))
           (line (cdr (first folds)))
           (fold?  (lambda (p)  (case axis
                                  (y (> (cdr p) line))
                                  (x (> (car p) line)))))
           (retained (remove-if fold? points))
           (folded (remove-if-not fold? points)))
      (loop for (x . y) in folded
            for reflection = (case axis
                               (y (cons x (- line (- y line))))
                               (x (cons (- line (- x line)) y)))
            do (setf retained (adjoin reflection retained :test 'equal))
            finally (return (length retained))))))

(defun points->grid (points)
          (loop with m = (1+ (reduce #'max (mapcar (lambda (p) (cdr p)) points)))
                with n = (1+ (reduce #'max (mapcar (lambda (p) (car p)) points)))
                with grid = (make-array (list m n))
                for p in points
                for x = (car p) and y = (cdr p)
                do (setf (aref grid y x) 1)
                finally (return grid)))

(defun advent-13b (filename)
  (multiple-value-bind (points folds) (parse-input filename)
    (loop for (axis . line) in folds
          for fold? = (lambda (p) (case axis
                                    (y (> (cdr p) line))
                                    (x (> (car p) line))))

          for remaining = points then retained

          for retained = (remove-if fold? points)
          then (remove-if fold? retained)

          for folded = (remove-if-not fold? points)
          then (remove-if-not fold? remaining)

          do (loop for (x . y) in folded
                   for reflection = (case axis
                                      (y (cons x (- line (- y line))))
                                      (x (cons (- line (- x line)) y)))
                   collect reflection into reflections
                   finally (setf retained (union retained reflections :test 'equal)))
          finally (print-grid (points->grid retained)))))
