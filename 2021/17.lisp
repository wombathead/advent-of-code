
;; ----------------------
;;   Day 17: Trick Shot
;; ----------------------

;;; TODO: make code not so disgustingly brute-force and hacky

(load "util.lisp")

(defun within-p (x y box)
  (let ((x-min (aref box 0))
        (x-max (aref box 1))
        (y-min (aref box 2))
        (y-max (aref box 3)))
    (and (>= x x-min) (<= x x-max)
         (>= y y-min) (<= y y-max))))

(defun lands-within-p (points box)
  (loop for (x . y) in points
        if (within-p x y box)
        return t))

(defun plot-points (start velocity x-max y-min)
  (let ((s-x (aref start 0))
        (s-y (aref start 1))
        (v-x (aref velocity 0))
        (v-y (aref velocity 1)))
    (loop for x = (+ s-x v-x) then (+ x v-x)
          for y = (+ s-y v-y) then (+ y v-y)
          until (or (> x x-max) (< y y-min))
          collect (cons x y) into p
          do
          (decf v-x (signum v-x))
          (decf v-y)
          finally (return (cons (cons s-x s-y) p)))))

(defun advent-17a (filename)
  (let ((input (first (get-file filename)))
        (start #(0 0))
        target-box)

    (multiple-value-bind (s matches)
      (ppcre:scan-to-strings
        "x=(-?[0-9]+)\.\.(-?[0-9]+), y=(-?[0-9]+)\.\.(-?[0-9]+)"
        input)
      (declare (ignore s))
      (setf target-box (map 'vector #'parse-integer matches))

      (loop with x-max = (aref target-box 1) and y-min = (aref target-box 2)
            for v-x from 0 upto x-max
            for m = (loop for v-y from 0 upto 1000
                          for velocity = (vector v-x v-y)
                          for points = (plot-points start velocity x-max y-min)
                          if (lands-within-p points target-box)
                          maximize (reduce #'max (mapcar (lambda (p) (cdr p)) points)))
            collect m into maxima
            finally (return (reduce #'max maxima))))))

(defun advent-17b (filename)
  (let ((input (first (get-file filename)))
        (start #(0 0))
        target-box)

    (multiple-value-bind (s matches)
      (ppcre:scan-to-strings
        "x=(-?[0-9]+)\.\.(-?[0-9]+), y=(-?[0-9]+)\.\.(-?[0-9]+)"
        input)
      (declare (ignore s))
      (setf target-box (map 'vector #'parse-integer matches))

      (loop with x-max = (aref target-box 1) and y-min = (aref target-box 2)
            for v-x from 0 upto x-max
            sum (loop for v-y from -1000 upto 1000
                      for velocity = (vector v-x v-y)
                      for points = (plot-points start velocity x-max y-min)
                      count (lands-within-p points target-box))))))
