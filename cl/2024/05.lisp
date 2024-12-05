
(in-package #:aoc)


(flet ((parse-input (filename)
         (iter
           (with active = 'rules)
           (with dag = (make-hash-table))
           (for line in (read-from-file filename))

           (when (empty-string-p line)
             (setf active 'updates)
             (next-iteration))

           (case active
             (rules
              (ppcre:do-register-groups ((#'parse-integer x y))
                  ("(\\d+)\\|(\\d+)" line)
                (push y (gethash x dag))
                (unless (gethash y dag)
                  (setf (gethash y dag) nil))))

             (updates
              (collect (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line)) into updates)))

           (finally (return (list dag updates)))))


       (update-valid-p (list precedence-graph)
         (flet ((number-valid (x)
                  (or (null (gethash x precedence-graph))
                      (loop for y in (gethash x precedence-graph)
                            unless (null (position y list))
                              if (> (position x list) (position y list)) return nil
                                finally (return t)))))
           (every (lambda (n) (number-valid n)) list))))

  (defun aoc-2024-05a (filename)
    (destructuring-bind (precedence-graph updates) (parse-input filename)
      (iter
        (for update in updates)
        (for midpoint = (nth (floor (length update) 2) update))
        (when (update-valid-p update precedence-graph)
          (sum midpoint)))))

  (defun aoc-2024-05b (filename)
    (destructuring-bind (precedence-graph updates) (parse-input filename)
      (iter
        (for update in updates)
        (unless (update-valid-p update precedence-graph)
          (let* ((subgraph (graph-node-restriction precedence-graph update))
                 (ordering (define-ordering> (topological-sort subgraph)))
                 (sorted (sorted update ordering))
                 (midpoint (nth (floor (length update) 2) sorted)))
            (sum midpoint)))))))
