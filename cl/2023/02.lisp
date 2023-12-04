;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Day 2: Cube Conundrum ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aoc)

(defun line->subgames (line)
  (ppcre:split ";" line))

(defun parse-subgame (subgame)
  (loop for color in '("red" "green" "blue")
        for regex = (format nil "\\d+ ~A" color)
        for match = (ppcre:scan-to-strings regex subgame)
        for (n c) = (ppcre:split " " match)
        collect (cons color (if match (parse-integer n) 0))))

(defun subgame-total-color-balls (subgame color)
  "Total balls of COLOR in SUBGAME"
  (rest (assoc color subgame :test 'equal)))

(defun game-possible-for-color-p (subgames color totals)
  (every (lambda (subgame)
           (<= (subgame-total-color-balls subgame color)
               (subgame-total-color-balls totals color)))
         subgames))

(defun aoc-2023-02a (filename)
  (loop with totals = '(("red" . 12) ("green" . 13) ("blue" . 14))
        with colors = '("red" "green" "blue")
        for line in (read-from-file filename)
        for subgames = (mapcar #'parse-subgame (ppcre:split ";" line))
        for i from 1
        if (every (lambda (color) (game-possible-for-color-p subgames color totals)) colors)
          sum i))

(defun aoc-2023-02b (filename)
  (loop with colors = '("red" "green" "blue")
        for line in (read-from-file filename)
        for subgames = (mapcar #'parse-subgame (ppcre:split ";" line))
        for minimums = (mapcar (lambda (color)
                                 (reduce #'max (mapcar (lambda (subgame)
                                                         (subgame-total-color-balls subgame color))
                                                       subgames)))
                               colors)
        sum (reduce #'* minimums)))
