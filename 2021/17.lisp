
;; ----------------------
;;   Day 17: Trick Shot
;; ----------------------

;;; TODO: make code not so disgustingly brute-force and hacky

(load "util.lisp")

(defun within-p (x y box)
  (let ((x-min (aref box 0))
        (y-min (aref box 1))
        (x-max (aref box 2))
        (y-max (aref box 3)))
    (and (>= x x-min) (<= x x-max)
         (>= y y-min) (<= y y-max))))

(defun lands-within-p (points box)
  (loop for (x . y) in points
        if (within-p x y box)
        return t
        finally (return nil)))

(defun plot-points (s-x s-y v-x v-y x-max y-min)
  (loop for x = (+ s-x v-x) then (+ x v-x)
        for y = (+ s-y v-y) then (+ y v-y)
        until (or (> x x-max) (< y y-min))
        collect (cons x y) into p
        do
        (decf v-x (signum v-x))
        (decf v-y)
        finally (return (cons (cons s-x s-y) p))))

(defun advent-17a (filename)
  (let ((input (first (get-file filename)))
        (s-x 0) (s-y 0)
        target-box)

    (multiple-value-bind (s matches)
      (ppcre:scan-to-strings
        "x=(-?[0-9]+)\.\.(-?[0-9]+), y=(-?[0-9]+)\.\.(-?[0-9]+)"
        input)
      (declare (ignore s))
      (setf matches (map 'vector #'parse-integer matches)
            target-box (vector (aref matches 0)
                               (aref matches 2)
                               (aref matches 1)
                               (aref matches 3)))

      (loop with highest = 0 and winner
            for v-x from 0 upto 2000
            do (loop for v-y from 0 upto 2000
                     for points = (plot-points s-x s-y
                                               v-x v-y
                                               (aref target-box 2)
                                               (aref target-box 1))
                     for max-y = (reduce #'max (mapcar (lambda (p) (cdr p)) points))

                     if (and (lands-within-p points target-box)
                             (> max-y highest))
                     do (setf highest max-y
                              winner (cons v-x v-y)))
            finally (return highest)))))

(defun advent-17b (filename)
  (let ((input (first (get-file filename)))
        (s-x 0) (s-y 0)
        target-box)

    (multiple-value-bind (s matches)
      (ppcre:scan-to-strings
        "x=(-?[0-9]+)\.\.(-?[0-9]+), y=(-?[0-9]+)\.\.(-?[0-9]+)"
        input)
      (declare (ignore s))
      (setf matches (map 'vector #'parse-integer matches)
            target-box (vector (aref matches 0)
                               (aref matches 2)
                               (aref matches 1)
                               (aref matches 3)))

      (loop for v-x from 0 upto 1000
            sum (loop for v-y from -1000 upto 1000
                      for points = (plot-points s-x s-y
                                                v-x v-y
                                                (aref target-box 2)
                                                (aref target-box 1))
                      count (lands-within-p points target-box))))))
