;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; If You Give A Seed A Fertilizer ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun parse-input-05 (filename)
  (loop with firstline = (first (read-from-file filename))
        with seeds = (mapcar #'parse-integer
                             (str:split " " (subseq firstline (1+ (search " " firstline)))))
        with current-map and maps
        for line in (nthcdr 2 (read-from-file filename))

        if (empty-string-p line)
          do (push current-map maps)
          and do (setf current-map nil)
        else
          unless (search ":" line)
            do (push (mapcar #'parse-integer (str:words line)) current-map)
        finally (return (list seeds (reverse maps)))))

(defun convert-number (x ranges)
  (cond ((null ranges) x)
        (t (destructuring-bind (d s r) (first ranges)
             (if (and (>= x s) (< x (+ s r)))
                 (+ x (- d s))
                 (convert-number x (rest ranges)))))))

(defun aoc-2023-05a (filename)
  (loop with (initial-seeds maps) = (parse-input-05 filename)
        for map in maps
        for seeds = (mapcar (lambda (s) (convert-number s map)) initial-seeds)
          then (mapcar (lambda (s) (convert-number s map)) seeds)
        finally (return (reduce #'min seeds))))

(defun compute-location (x maps)
  (loop for map in maps
        for y = (convert-number x map) then (convert-number y map)
        finally (return y)))

(defun aoc-2023-05b (filename)
  (loop with (seed-ranges maps) = (parse-input-05 filename)
        with ranges = (loop for (start length) on seed-ranges by #'cddr
                            collect (list start length))
        with tested = (make-hash-table)
        for (start length) in ranges
        minimize (loop for i from start repeat length
                         minimize (compute-location i maps))))
