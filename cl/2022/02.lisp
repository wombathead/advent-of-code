;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 2: Rock Paper Scissors ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun char-difference (a b)
  (- (char-code a) (char-code b)))

(defun char->action (c)
  (let ((c (if (char> c #\C)
               (code-char (+ (char-code #\A)
                             (char-difference c #\X)))
               c)))
    (char-difference c #\A)))

(defparameter rps-matrix #2A((3 0 6)
                             (6 3 0)
                             (0 6 3)))

(defun aoc-2022-02a (filename)
  (loop for line in (read-from-file filename)
        for opponent = (char->action (char line 0))
        for response = (char->action (char line 2))
        sum (+ 1 response (aref rps-matrix response opponent))))

(defun aoc-2022-02b (filename)
  (loop for line in (read-from-file filename)
        for opponent = (char->action (char line 0))
        for outcome = (char->action (char line 2))
        for response = (case outcome
                         (0 (mod (+ opponent 2) 3)) ;; lose
                         (1 opponent) ;; draw
                         (2 (mod (+ opponent 1) 3))) ;; win
        sum (+ 1 response (aref rps-matrix response opponent))))
