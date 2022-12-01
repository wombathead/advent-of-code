;; ---------------------------- ;;
;;  UTILITY/GENERAL FUNCTIONS   ;;
;; ---------------------------- ;;

(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :parse-number)
(ql:quickload :str)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))

(define-modify-macro mulf (x) *)

(defun my-xor (a b)
  (and
    (or a b)
    (not (and a b))))

(defun within-range (num lower upper)
  "return T if NUM is between LOWER and UPPER"
  (and (>= num lower) (<= num upper)))

(defun within-all-ranges (num ranges)
  (not (member NIL (mapcar #'(lambda (r) (within-range num (first r) (second r)))
                           ranges))))

(defun within-some-range (num ranges)
  (member t (mapcar #'(lambda (r) (within-range num (first r) (second r)))
                    ranges)))

(defun binary-to-dec (bin-str zero-char lo hi)
  "convert binary string BIN-STR with ZERO-CHAR representing 0 to decimal value"
  (if (= (length bin-str) 1)
      (if (eql zero-char (char bin-str 0))
          lo
          (1- hi))
      (if (eql zero-char (char bin-str 0))
          (binary-to-dec (subseq bin-str 1)
                         zero-char
                         lo
                         (floor (+ hi lo) 2))
          (binary-to-dec (subseq bin-str 1)
                         zero-char
                         (floor (+ hi lo) 2)
                         hi))))

(defun degrees-to-radians (x)
  (* x (/ pi 180)))

(defun contiguous-sum (lst target)
  "find a contiguous sequence of items in LST that sum to TARGET "
  (let ((window (list (first lst)))
        (i 0) (j 1)
        (n (length lst)))
    (loop while T do
          (loop while (and (< (reduce #'+ window) target)
                           (<= j n)) do
                (setf window (subseq lst i (1+ (incf j)))))

          (if (= (reduce #'+ window) target)
              (return-from contiguous-sum window))

          (loop while (> (reduce #'+ window) target) do
                (setf window (subseq lst (incf i) (1+ j)))))))

(defmacro vec-x (vec) `(aref ,vec 0))
(defmacro vec-y (vec) `(aref ,vec 1))

(defun lp-norm (x p)
  (expt (reduce #'+ (map 'vector (lambda (u) (expt (abs u) p)) x)) (/ 1 p)))

(defun l1-norm (x)
  (lp-norm x 1))

(defun vec+ (u v)
  (map 'vector #'+ u v))

(defun vec- (u v)
  (map 'vector #'- u v))

(defun vec* (k u)
  (map 'vector (lambda (elem) (* elem k)) u))

