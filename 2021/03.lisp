
;; ----------------------------
;;   Day 3: Binary Diagnostic 
;; ----------------------------

(load "util.lisp")

(defun advent-03a (filename)
  (loop with input = (get-file filename)
        with n = (length (first input))
        with m = (length input)
        for i from 0 below n
        with gamma = 0
        for zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) input))
        do (setf gamma (if (>= zeroes (/ m 2)) (* gamma 2) (1+ (* gamma 2))))
        finally (return (* gamma (logxor (1- (expt 2 n)) gamma)))))

(defun advent-03b (filename)
  (flet ((nth-char= (string n character)
           (char= (char string n) character)))
    (loop with input = (get-file filename)
          with n = (length (first input))
          for i from 0 below n

          with generator = input 
          for generator-m = (length generator)
          for generator-zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) generator))
          for generator-ones = (- generator-m generator-zeroes)
          for generator-char = (if (> generator-zeroes generator-ones) #\0 #\1)  

          with scrubber = input 
          for scrubber-m = (length scrubber)
          for scrubber-zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) scrubber))
          for scrubber-ones = (- scrubber-m scrubber-zeroes)
          for scrubber-char = (if (<= scrubber-zeroes scrubber-ones) #\0 #\1)  

          unless (= 1 generator-m)
          do (setf generator (remove-if-not (lambda (bitstring) (nth-char= bitstring i generator-char))
                                            generator))
          unless (= 1 scrubber-m)
          do (setf scrubber (remove-if-not (lambda (bitstring) (nth-char= bitstring i scrubber-char))
                                           scrubber)) 
          finally (return (* (parse-integer (first generator) :radix 2)
                             (parse-integer (first scrubber) :radix 2))))))
