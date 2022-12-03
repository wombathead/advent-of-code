
;; ----------------------------
;;   Day 3: Binary Diagnostic 
;; ----------------------------

(in-package :aoc)

(defun aoc-2021-03a (filename)
  (loop with input = (read-from-file filename)
        with n = (length (first input))
        with m = (length input)
        for i from 0 below n
        with gamma = 0
        for zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) input))
        do (setf gamma (if (>= zeroes (/ m 2)) (ash gamma 1) (1+ (ash gamma 1))))
        finally (return (* gamma (logxor (1- (expt 2 n)) gamma)))))

(defun aoc-2021-03b (filename)
  (flet ((filter-candidates (candidates filter-rule)
           (loop for i from 0 below (length (first candidates))
                 with remaining = candidates
                 for zeroes = (count #\0 (mapcar (lambda (bitstring) (char bitstring i)) remaining))
                 for ones = (- (length remaining) zeroes)
                 for char = (funcall filter-rule zeroes ones)
                 until (= 1 (length remaining))
                 do (setf remaining (remove-if-not (lambda (bitstring)
                                                     (nth-char= bitstring i char))
                                                   remaining))
                 finally (return (first remaining)))))

    (let ((input (read-from-file filename)))
      (* (parse-integer
           (filter-candidates input (lambda (zeroes ones) (if (> zeroes ones) #\0 #\1)))
           :radix 2)
         (parse-integer
           (filter-candidates input (lambda (zeroes ones) (if (<= zeroes ones) #\0 #\1)))
           :radix 2)))))
