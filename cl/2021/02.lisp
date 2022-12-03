
;; ----------------
;;   Day 2: Dive!
;; ----------------

(in-package :aoc)

(defun aoc-2021-02a (filename)
  (loop for (dir dst) in (mapcar #'str:words (read-from-file filename))
        for direction = (read-from-string dir)
        for distance = (parse-integer dst)
        with (x y) = '(0 0)
        do (case direction
             (forward (incf x distance))
             (down (incf y distance))
             (up (decf y distance)))
        finally (return (* x y))))

(defun aoc-2021-02b (filename)
  (loop for (dir dst) in (mapcar #'str:words (read-from-file filename))
        for direction = (read-from-string dir)
        for distance = (parse-integer dst)
        with (x y aim) = '(0 0 0)
        do (case direction
             (forward (incf x distance)
                      (incf y (* aim distance)))
             (down (incf aim distance))
             (up (decf aim distance)))
        finally (return (* x y))))
