
;; ----------------
;;   Day 2: Dive!
;; ----------------

(load "util.lisp")

(defun advent-02a (filename)
  (loop for (dir dst) in (mapcar #'str:words (get-file filename))
        for direction = (read-from-string dir)
        for distance = (parse-integer dst)
        with (x y) = '(0 0)
        do (case direction
             (forward (incf x distance))
             (down (incf y distance))
             (up (decf y distance)))
        finally (return (* x y))))

(defun advent-02b (filename)
  (loop for (dir dst) in (mapcar #'str:words (get-file filename))
        for direction = (read-from-string dir)
        for distance = (parse-integer dst)
        with (x y aim) = '(0 0 0)
        do (case direction
             (forward (incf x distance)
                      (incf y (* aim distance)))
             (down (incf aim distance))
             (up (decf aim distance)))
        finally (return (* x y))))
