
(defun get-file (filename)
  (with-open-file (stream filename)
	(loop for line = (read-line stream NIL)
		  while line
		  collect line)))

