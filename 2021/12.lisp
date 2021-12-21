
;; ---------------------------
;;   Day 12: Passage Pathing
;; ---------------------------

(load "util.lisp")

(defun advent-12a (filename)
  (flet ((count-paths (G start end)
           (let ((paths 0))
             (labels ((explore (u explored)
                        (if (funcall (hash-table-test G) u end)
                            (incf paths)
                            (let ((explored (alexandria:copy-hash-table explored)))
                              (unless (every #'upper-case-p u)
                                (setf (gethash u explored) t))
                              (loop for v in (gethash u G)
                                    unless (gethash v explored)
                                    do (explore v explored))))))

               (explore start (make-hash-table :test (hash-table-test G)))
               paths))))

    (let ((G (make-hash-table :test 'equal)))
      (loop for line in (get-file filename)
        for (u v) = (str:split "-" line)
        do
        (push v (gethash u G))
        (push u (gethash v G)))

      (count-paths G "start" "end"))))

(defun advent-12b (filename)
  (flet ((count-paths (G start end)
           (let ((paths 0))
             (labels ((explore (u explored extra-explore)
                        (if (funcall (hash-table-test G) u end)
                            (incf paths)
                            (let ((explored (alexandria:copy-hash-table explored)))
                              (unless (every #'upper-case-p u)
                                (setf (gethash u explored) t))
                              (loop for v in (gethash u G)
                                    do (cond ((not (gethash v explored))
                                              (explore v explored extra-explore))
                                             ((and extra-explore (gethash v explored))
                                              (explore v explored nil))))))))

               (explore start (make-hash-table :test (hash-table-test G)) t)
               paths))))

    (let ((G (make-hash-table :test 'equal)))
      (loop for line in (get-file filename)
        for (u v) = (str:split "-" line)
        do (cond ((string= u "start") (push v (gethash "start" G)))
                 ((string= v "start") (push u (gethash "start" G)))
                 ((string= u "end") (push "end" (gethash v G)))
                 ((string= v "end") (push "end" (gethash u G)))
                 (t (push v (gethash u G)) (push u (gethash v G)))))

      (count-paths G "start" "end"))))
