;; ---------------------------- ;;
;;   DAY 19: MONSTER MESSAGES 
;; ---------------------------- ;;

(load "util.lisp")

(defun parse-rules (input)
  (loop
    with grammar = (make-hash-table)
    for rule = (pop input) 
    until (string= rule "")
    for rule-number = (parse-integer (first (str:split ": " rule)))
    and productions = (str:split " | " (second (str:split ": " rule)))
    do 
    (setf (gethash rule-number grammar) productions)
    finally (return grammar)))

(defun day19a (file)
  (let* ((input (get-file file))
         (grammar (parse-rules input)))
    (maphash (lambda (k v) (format t "h[~D]: ~S~%" k v)) grammar))
  )

(defun build-language (grammar)
  (let (language)
    (flet ((derive (rule)
             (if )
             )))
    )
  )

(alexandria:map-product
  (lambda (&rest lists)
    (reduce )
    (apply (lambda (l m) (concatenate 'string l m)) lists) lists)
  '("AB" "BC") '("C" "D" "E"))

