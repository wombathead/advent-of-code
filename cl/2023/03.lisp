;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 3: Gear Ratios ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun is-empty-space-p (string)
  (string= string "."))

(defun is-part-number-p (string)
  (and string (every #'digit-char-p string)))

(defun is-symbol-p (value)
  (not (or
        (null value)
        (is-part-number-p value)
        (is-empty-space-p value))))

(defun parse-input (filename)
  (loop with grid = (make-hash-table :test 'equal)
        for line in (read-from-file filename)
        for y from 0
        for matches = (ppcre:all-matches "\\d+" line)
        do
           ;; store all part numbers
           (loop for (start end) on matches by #'cddr
                 for prefix = (loop repeat y collect #\0 into zeroes finally (return (coerce zeroes 'string)))
                 for number = (concatenate 'string prefix (subseq line start end))
                 do (loop for i from start below end
                          for coordinate = (cons i y)
                          do (setf (gethash coordinate grid) number)))

           ;; store all symbols
           (loop for c across line
                 for x from 0
                 for string = (subseq line x (1+ x))
                 for coordinate = (cons x y)
                 unless (or (is-part-number-p string) (is-empty-space-p string))
                   do (setf (gethash coordinate grid) string))

        finally (return grid)))

(defun adjacent-to-symbol-p (ht x y)
  (loop for dy in '(-1 0 1)
        count (loop for dx in '(-1 0 1)
                    for coord = (cons (+ x dx) (+ y dy))
                    for value = (gethash coord ht)
                    when (is-symbol-p value) return t
                      finally (return nil))
          into adjacent-symbols
        finally (return (plusp adjacent-symbols))))

(defun adjacent-parts (ht x y)
  (loop for dy in '(-1 0 1)
        nconc (loop for dx in '(-1 0 1)
                    for coord = (cons (+ x dx) (+ y dy))
                    for string = (gethash coord ht)
                    when (is-part-number-p string) collect string)
          into part-numbers
        finally (return (remove-duplicates part-numbers :test 'equal))))

(defun is-gear (ht x y)
  (let ((string (gethash (cons x y) ht))
        (adjacent-parts (adjacent-parts ht x y)))
    (when (and (string= string "*")
               (= 2 (length adjacent-parts)))
      adjacent-parts)))

(defun aoc-2023-03a (filename)
  (loop with lines = (read-from-file filename)
        with grid = (parse-input filename)
        for coord in (hash-table-keys grid)
        for (x . y) = coord
        for string = (gethash coord grid)
        if (and (adjacent-to-symbol-p grid x y)
                (is-part-number-p string))
          collect string into part-numbers
        ;; TODO: errr this works for the wrong reason
        finally (return (reduce #'+ (mapcar #'parse-integer (remove-duplicates part-numbers))))))

(defun aoc-2023-03b (filename)
  (loop with lines = (read-from-file filename)
        with grid = (parse-input filename)
        for coord in (hash-table-keys grid)
        for (x . y) = coord
        for string = (gethash coord grid)
        if (is-gear grid x y)
          sum (reduce #'* (mapcar #'parse-integer (adjacent-parts grid x y)))))
