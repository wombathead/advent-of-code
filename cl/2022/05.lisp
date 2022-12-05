;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 5: Supply Stacks ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun column->stack-number (column)
  (1+ (floor column 4)))

(defun parse-stacks (filename)
  (loop with stacks = (make-hash-table)
        for line in (read-from-file filename)
        for indices = (ppcre:all-matches "\\[[A-Z]\\]" line)
        do (loop for (s e) on indices by #'cddr
                 for item = (char line (1+ s))
                 for stack = (column->stack-number s)
                 do (push item (gethash stack stacks)))
        finally
        (loop for k in (hash-table-keys stacks)
              do (setf (gethash k stacks) (reverse (gethash k stacks))))
        (return stacks)))

(defun parse-instructions (filename)
  (loop with blankline
        for line in (read-from-file filename)
        if (string= line "")
        do (setf blankline t)
        if blankline
        collect (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line))
        into instructions
        finally (return (rest instructions))))

(defun aoc-2022-05a (filename)
  (loop with stacks = (parse-stacks filename)
        for (n from to) in (parse-instructions filename)
        do (dotimes (_ n)
             (push (pop (gethash from stacks)) (gethash to stacks)))
        finally (return (chars->string (mapcar (lambda (s) (first (gethash s stacks)))
                                               (sort (hash-table-keys stacks) #'<))))))

(defun aoc-2022-05b (filename)
  (loop with stacks = (parse-stacks filename)
        for (n from to) in (parse-instructions filename)
        for substack = (loop repeat n
                             collect (pop (gethash from stacks)))
        do (setf (gethash to stacks) (append substack (gethash to stacks)))
        finally (return (chars->string (mapcar (lambda (s) (first (gethash s stacks)))
                                               (sort (hash-table-keys stacks) #'<))))))
