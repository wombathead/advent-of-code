;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 7: Bridge Repair ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:aoc)


(defun find-assignment (target numbers operators)
  (labels ((find-assignments (remaining assignment)
             (case (length remaining)
               (1 (when (= target (first remaining))
                    (reverse assignment)))

               (otherwise
                (iter
                  (for op in operators)
                  (for (x y . rest) = remaining)
                  (for ops = (find-assignments
                              (cons (funcall op x y) rest)
                              (cons op assignment)))
                  (when ops
                    (collect ops)))))))
    (find-assignments numbers nil)))


(defun aoc-2024-07a (filename)
  (iter
    (for line in (read-from-file filename))
    (ppcre:do-register-groups ((#'parse-integer target) numbers)
        ("(\\d+): (.*)" line)
      (let ((numbers (mapcar #'parse-integer (str:words numbers))))
        (when (find-assignment target numbers '(* +))
          (sum target))))))


(let ((concat (lambda (x y) (parse-integer (format nil "~S~S" x y)))))
  (defun aoc-2024-07b (filename)
    (iter
      (for line in (read-from-file filename))
      (ppcre:do-register-groups ((#'parse-integer target) numbers)
          ("(\\d+): (.*)" line)
        (let ((numbers (mapcar #'parse-integer (str:words numbers))))
          (when (find-assignment target numbers `(* + ,concat))
            (sum target)))))))
