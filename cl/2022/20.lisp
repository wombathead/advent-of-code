;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 20: Grove Positioning System ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun whereis (item list n &optional (test 'equal))
  (loop for v in list
        for i from 0 below n
        if (funcall test item v) return i))

(defun insert-at (idx item list)
  (let ((list (copy-list list)))
    (if (plusp idx)
        (push item (cdr (nthcdr (1- idx) list)))
        (setf list (cons item list)))
    list))

(defun aoc-2022-20a (filename)
  (loop with input = (mapcar #'parse-integer (read-from-file filename))
        with n = (length input)
        with data = (loop with counts = (make-hash-table)
                          for n in input
                          do (if (gethash n counts)
                                 (incf (gethash n counts))
                                 (setf (gethash n counts) 1))
                          collect (cons n (gethash n counts)))
        with list = (copy-list data)

        for d in data
        for (x . r) = d
        for i = (whereis d list n)
        for j = (mod (+ i x) (1- n))
        do (setf list (remove d list)
                 list (insert-at j d list))
        finally
        (let ((i (whereis '(0 . 1) list n)))
          (return (reduce #'+ (mapcar #'car (mapcar
                                              (lambda (m)
                                                (nth (mod (+ i (* m 1000)) n) list))
                                              '(1 2 3))))))))

(defun aoc-2022-20b (filename)
  (loop with input = (mapcar #'parse-integer (read-from-file filename))
        with n = (length input)
        with key = 811589153
        with data = (loop with counts = (make-hash-table)
                          for n in input
                          do (if (gethash n counts)
                                 (incf (gethash n counts))
                                 (setf (gethash n counts) 1))
                          collect (cons (* n key) (gethash n counts)))
        with list = (copy-list data)
        repeat 10
        do (loop for d in data
                 for (x . r) = d
                 for i = (whereis d list n)
                 for j = (mod (+ i x) (1- n))
                 do (setf list (remove d list)
                          list (insert-at j d list)))
        finally
        (let ((i (whereis '(0 . 1) list n)))
          (return (reduce #'+ (mapcar #'car (mapcar
                                              (lambda (m)
                                                (nth (mod (+ i (* m 1000)) n) list))
                                              '(1 2 3))))))))
