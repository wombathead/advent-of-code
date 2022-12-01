
(load "util.lisp")

(defun make-btree-node (&optional data left right)
  (list data left right))

(defmacro btree-root (node)
  `(first ,node))

(defmacro btree-left (node)
  `(second ,node))

(defmacro btree-right (node)
  `(third ,node))

(defun btree-insert-left (tree item)
  (unless tree (setf tree (make-btree-node)))
  (if (listp item)
      (setf (btree-left tree) (btree-insert-left (btree-left tree) (first item))
            (btree-right tree) (btree-insert-right (btree-right tree) (second item)))
      (setf (btree-root tree) (make-btree-node item)))
  tree)

(defun btree-insert-right (tree item)
  (unless tree (setf tree (make-btree-node)))
  (if (listp item)
      (setf (btree-left tree) (btree-insert-left tree (first item))
            (btree-right tree) (btree-insert-right tree (second item)))
      (setf (btree-right tree) (make-btree-node item)))
  tree)

(defun inorder (tree)
  "Left Root Right"
  (princ "(")
  (when (btree-left tree) (inorder (btree-left tree)))
  (when (first tree) (format t "~A" (first tree)))
  (when (btree-right tree) (inorder (btree-right tree)))
  (princ ")"))

(defun preorder (tree)
  "Root Left Right"
  (princ "(")
  (when (first tree) (format t "~A" (first tree)))
  (when (btree-left tree) (preorder (btree-left tree)))
  (when (btree-right tree) (preorder (btree-right tree)))
  (princ ")"))

(defun parse-line (line)
  (let ((expression (copy-seq line)))
    (loop for c across expression
          for i from 0
          do (case c
               (#\[ (setf (char expression i) #\())
               (#\] (setf (char expression i) #\)))
               (#\, (setf (char expression i) #\Space))))
    (setf expression (read-from-string expression))
    ()
    ))
#|
(let* ((width 50))
  (loop with m = (floor width 2) 
        for level from 0 
        for print-start = (- m level)
        for level-width = (1+ (* 2 level))
        do (loop repeat print-start do (format t "."))
        do (loop repeat level-width do (princ "+"))
        do (loop for i from (+ print-start level-width) upto width do (format t ".")) 
        (terpri)
        until (= print-start 0)))
|#


