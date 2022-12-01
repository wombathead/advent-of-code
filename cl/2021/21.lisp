
(load "util.lisp")

(defun advent-21a (filename)
  (let* ((input (get-file filename))
         (a0 (aref (nth-value 1 (ppcre:scan-to-strings "position: ([0-9]+)"
                                                       (first input))) 0))
         (b0 (aref (nth-value 1 (ppcre:scan-to-strings "position: ([0-9]+)"
                                                       (second input))) 0)))

    (loop with positions = (vector (1- (parse-integer a0)) (1- (parse-integer b0)))
          with scores = (vector 0 0)
          for dice = 0 then (mod (+ dice 3) 100)
          for rolled = (loop for i from 0 below 3 sum (1+ (mod (+ dice i) 100)))
          for rolls = 3 then (+ 3 rolls)
          for player = 0 then (- 1 player)
          do 
          (setf (aref positions player) (mod (+ (aref positions player) rolled) 10))
          (incf (aref scores player) (1+ (aref positions player)))
          until (>= (aref scores player) 1000)
          finally (return (* (aref scores (- 1 player)) rolls)))))

(defun advent-21b (filename)
  (let* ((input (get-file filename))
         (a0 (aref (nth-value 1 (ppcre:scan-to-strings "position: ([0-9]+)"
                                                       (first input))) 0))
         (b0 (aref (nth-value 1 (ppcre:scan-to-strings "position: ([0-9]+)"
                                                       (second input))) 0))
         (positions (make-array '(2 10)))   ; #pos-i = pos[i+1]
         (scores (make-array '(2 22))))     ; #score-i = scores[i]

    (setf (aref positions 0 (1- (parse-integer a0))) 1
          (aref positions 1 (1- (parse-integer b0))) 1)
    
    (loop with rolls = (mapcar (lambda (l) (reduce #'+ l))
                               (alexandria:map-product #'list '(1 2) '(1 2)))

          for player = 0 then (- 1 player)
          do (loop for r in rolls
                   ;; update all positions
                   do (loop with old-positions = (alexandria:copy-array positions)
                            for p from 0 below 10
                            for new-pos = (mod (+ p r) 10)
                            for delta = (aref old-positions player p)  
                            do
                            (incf (aref positions player new-pos) delta)
                            (decf (aref positions player p) delta))
                   (format t "~A~%" positions))
          repeat 1)))
