;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 2: Red-Nosed Reports ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:aoc)

(flet ((get-reports (filename)
         (loop for line in (read-from-file filename)
               collect (mapcar #'parse-integer (str:words line))))

       (report-safe-p (report)
         (and (monotonep report :strict t)
              (loop for (x y) on report while y
                    for diff = (abs (- x y))
                    if (or (< diff 1) (> diff 3)) return nil
                    finally (return t)))))

  (defun aoc-2024-02a (filename)
    (loop for report in (get-reports filename)
          count (report-safe-p report)))

  (flet ((report-safe-p (report)
           (or (report-safe-p report)
               (loop for i from 0 upto (1- (length report))
                     for r = (remove-nth i report)
                     if (report-safe-p r) return t
                     finally (return nil)))))

    (defun aoc-2024-02b (filename)
      (loop for report in (get-reports filename)
            count (report-safe-p report)))))
