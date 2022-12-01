
;; -----------------------------------
;;   Day 14: Extended Polymerization
;; -----------------------------------

(load "util.lisp")

(defun parse-input (filename)
  (let* ((input (get-file filename))
         (template (first input))
         (rules (make-hash-table :test 'equal)))

    (setf rules (loop for rule in (cddr input)
                      for matches = (nth-value 1 (cl-ppcre:scan-to-strings
                                                   "([A-Z]+) -> ([A-Z]+)"
                                                   rule))
                      for lhs = (aref matches 0)
                      and rhs = (aref matches 1)
                      do (setf (gethash lhs rules) rhs)
                      finally (return rules)))
    (values template rules)))

(defun simulate-polymer (polymer rules steps)
  (let ((pair-counts (make-hash-table :test 'equal))
        (elem-counts (make-hash-table :test 'equal)))

    ;; initialise pair counts
    (loop for (a b) on (coerce polymer 'list) while b
          for pair = (coerce (list a b) 'string)
          do (if (gethash pair pair-counts) 
                 (incf (gethash pair pair-counts))
                 (setf (gethash pair pair-counts) 1)))

    ;; initialise element counts
    (loop for a across polymer
          for elem-string = (coerce (list a) 'string)
          do (if (gethash elem-string elem-counts) 
                 (incf (gethash elem-string elem-counts))
                 (setf (gethash elem-string elem-counts) 1)))

    (loop for i from 0 below steps
          for pc = (alexandria:copy-hash-table pair-counts)
          do (loop for pair in (alexandria:hash-table-keys pair-counts)
                   for c = (gethash pair pair-counts)
                   for substitution = (gethash pair rules)
                   if substitution
                   do (decf (gethash pair pc) c)   ; we have destroyed this pair

                   ;; introduce new element
                   (if (gethash substitution elem-counts)
                       (incf (gethash substitution elem-counts) c)
                       (setf (gethash substitution elem-counts) c))

                   ;; keep track of the new pair
                   (let ((new-pair-1 (concatenate 'string
                                                  (list (char pair 0))
                                                  substitution))
                         (new-pair-2 (concatenate 'string
                                                  substitution
                                                  (list (char pair 1)))))
                     (if (gethash new-pair-1 pc)
                         (incf (gethash new-pair-1 pc) c)
                         (setf (gethash new-pair-1 pc) c))
                     (if (gethash new-pair-2 pc)
                         (incf (gethash new-pair-2 pc) c)
                         (setf (gethash new-pair-2 pc) c))))
          (setf pair-counts pc)
          finally (return elem-counts))))

(defun advent-14a (filename)
  (multiple-value-bind (template rules) (parse-input filename)
    (let ((counts (simulate-polymer template rules 10)))
      (- (reduce #'max (mapcar (lambda (e) (gethash e counts))
                               (alexandria:hash-table-keys counts)))
         (reduce #'min (mapcar (lambda (e) (gethash e counts))
                               (alexandria:hash-table-keys counts)))))))
(defun advent-14b (filename)
  (multiple-value-bind (template rules) (parse-input filename)
    (let ((counts (simulate-polymer template rules 40)))
      (- (reduce #'max (mapcar (lambda (e) (gethash e counts))
                               (alexandria:hash-table-keys counts)))
         (reduce #'min (mapcar (lambda (e) (gethash e counts))
                               (alexandria:hash-table-keys counts)))))))
