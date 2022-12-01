
(load "util.lisp")

(defun hex->bin (hexstring)
  (let ((n (parse-integer hexstring :radix 16)))
    (loop with binstring
          for m = n then (floor m 2)
          until (zerop m)
          do (push (if (zerop (mod m 2)) #\0 #\1) binstring)
          finally (return (coerce binstring 'string)))))

(defun parse-literal (packet)
  (let ((packet (subseq packet 6)))
    (loop for i from 0 below (floor (length packet) 5)
          for start = (* i 5) for end = (+ start 5)
          for str = (concatenate 'string str (subseq packet (1+ start) end))
          finally (return (parse-integer str :radix 2)))))

(defun parse-operator (packet)
  (let* ((packet (subseq packet 6))
        (length-type-id (char packet 0))
        L)
    (setf L (subseq packet 1 (+ 1 (if (char= length-type-id #\0) 15 11))))
    (format t "length type: ~A, L: ~A (~D)~%"
            length-type-id L (parse-integer L :radix 2))))

(defun parse-packet (packet)
  (let ((version (parse-integer (subseq packet 0 3) :radix 2))
        (type-id (parse-integer (subseq packet 3 6) :radix 2)))

    (format t "V: ~D, T: ~D~%" version type-id)

    (case type-id
      (4 (parse-literal packet))
      (otherwise (parse-operator packet)))))
