(load "util.lisp")

; TODO: fix B

(defun parse-instructions (file)
  (let ((input (get-file file)))
    (mapcar #'(lambda (l)
                (list (read-from-string (subseq l 0 1))
                      (read-from-string (subseq l 1))))
            input)))

(defun move-position (position view direction distance)
  (let ((position (copy-seq position)))
    (case direction
      (N (incf (vec-y position) distance))
      (S (decf (vec-y position) distance))
      (E (incf (vec-x position) distance))
      (W (decf (vec-x position) distance))
      (F
        ; this works since view can only be cardinal directions but beware
        (incf (vec-x position) (* distance (vec-x view)))
        (incf (vec-y position) (* distance (vec-y view)))))
    position))

(defun rotate-90-ccw (vec)
  "rotate VEC 90 degrees CCW"
  (vector (- (vec-y vec)) (vec-x vec)))

(defun rotate-90-cw (vec)
  (vector (vec-y vec) (- (vec-x vec))))

(defun rotate-90-ccw-about (p q)
  "Rotate point P 90 degrees CCW about point Q"
  (let ((translated (vec- P Q)))
    (vec+ Q (rotate-90-ccw translated))))

(defun rotate-90-cw-about (p q)
  "Rotate point P 90 degrees CW about point Q"
  (let ((translated (vec- P Q)))
    (vec+ Q (rotate-90-cw translated))))

(defun rotate-view (view direction degrees)
  (let ((view (copy-seq view)))
    (flet ((k-turns (vec k)
             "perform K rotations of VEC counter-clockwise"
             (loop for v = vec then (rotate-90-ccw v)
                   for i from 0 below k
                   finally (return v))))
      (case direction
        (R (k-turns view (- 4 (/ degrees 90))))
        (L (k-turns view (/ degrees 90)))))))

(defun day12a (input)
  (let ((instructions (parse-instructions input)))
    (loop for instruction in instructions
          for direction = (first instruction)
          for distance = (second instruction)
          with ship-position = #(0 0) and view = #(1 0)
          do (if (member direction '(N S E W F))
                 (setf ship-position (move-position ship-position view direction distance))
                 (setf view (rotate-view view direction distance)))
          finally (return (l1-norm ship-position)))))

(defun day12b (input)
  (let ((instructions (parse-instructions input)))
    (loop for instruction in instructions
          for direction = (first instruction)
          for distance = (second instruction)
          with ship-position = #(0 0)
          with waypoint = #(10 1)
          do
          (cond ((member direction '(N S E W)) 
                 (setf waypoint (move-position waypoint nil direction distance)))

                ((member direction '(F))
                 (let ((diff (vec- waypoint ship-position)))
                   (setf ship-position (vec+ ship-position (vec* distance diff))
                         waypoint (vec+ ship-position diff))))

                ((member direction '(L R))
                 (case direction
                   (L (setf waypoint (loop for v = waypoint then (rotate-90-ccw-about
                                                                   waypoint ship-position) 
                                           for i from 0 below (/ distance 90)  
                                           finally (return v))))
                   (R (setf waypoint (loop for v = waypoint then (rotate-90-cw-about
                                                                   waypoint ship-position)
                                           for i from 0 below (/ distance 90) 
                                           finally (return v)))))))
          (format t "ship: ~A, wpt: ~A~%" ship-position waypoint)

          finally (return (l1-norm ship-position)))))

(defun day12 ()
  (format t "12: ~A, ~A~%" (day12a "input12.txt") (day12b "input12.txt"))
  )
