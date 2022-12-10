;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 10: Cathode-Ray Tube ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun aoc-2022-10a (filename)
  (loop with input = (mapcar #'str:words (read-from-file filename))
        with commands = (mapcar (lambda (c)
                                  (let ((timer (if (string= (first c) "addx") 1 0))
                                        (arg (if (second c) (parse-integer (second c)) 0)))
                                    (cons timer arg)))
                                input)
        with register = 1
        with cmd = (pop commands)
        for cycle from 1
        while cmd
        for (timer . arg) = cmd

        if (or (= cycle 20) (zerop (mod (+ cycle 20) 40)))
        sum (* cycle register)

        if (zerop timer)
        do (setf register (+ register arg)
                 cmd (pop commands))
        else do (decf (car cmd))))

(defun aoc-2022-10b (filename)
  (loop with input = (mapcar #'str:words (read-from-file filename))
        with commands = (mapcar (lambda (c)
                                  (let ((timer (if (string= (first c) "addx") 1 0))
                                        (arg (if (second c) (parse-integer (second c)) 0)))
                                    (cons timer arg)))
                                input)

        with dimensions = '(6 40)
        with (h w) = dimensions
        
        with image = (make-array dimensions)
        with register = 1
        with cmd = (pop commands)

        for cycle from 1
        for (timer . arg) = cmd

        for col = (mod (1- cycle) w)
        for row = (floor (1- cycle) w)

        until (= (1- cycle) (* w h))

        do (setf (aref image row col) (if (< (abs (- col register)) 2) #\# #\.))

        if (zerop timer)
        do (setf register (+ register arg)
                 cmd (pop commands))
        else do (decf (car cmd))

        finally (loop for r from 0 below h
                      do (loop for c from 0 below w
                               do (format t "~C" (aref image r c)))
                                  (terpri))))
