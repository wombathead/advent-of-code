
(ql:quickload :str)

(load "util.lisp")

(defun execute (program a b)
  "Execute PROGRAM with inputs A and B"
  (let ((program (copy-seq program)))
    (setf (aref program 1) a
          (aref program 2) b)
    (loop with ip = 0
          for instr = (aref program ip)
          while (/= instr 99)
          for a = (aref program (+ ip 1))
          for b = (aref program (+ ip 2))
          for c = (aref program (+ ip 3))
          do
          (case instr
            (1 (setf (aref program c) (+ (aref program a) (aref program b))))
            (2 (setf (aref program c) (* (aref program a) (aref program b)))))
          (incf ip 4)
          finally (return program))))

(defun program-output (program)
  (aref program 0))

(defun advent-02a (filename)
  (let ((program (map 'vector #'parse-integer (str:split "," (first (get-file filename))))))
    (program-output (execute program 12 2))))

(defun advent-02b (filename)
  (let ((program (map 'vector #'parse-integer (str:split "," (first (get-file filename))))))
    (loop for i from 0 upto 99
          for result = (loop for j from 0 upto 99
                             for output = (program-output (execute program i j))
                             if (= output 19690720)
                             return (+ (* 100 i) j))
          if result return result)))
