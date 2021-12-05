
;; ----------------------
;;   Day 4: Giant Squid
;; ----------------------

(load "util.lisp")

(ql:quickload :str)

(defun parse-numbers (filename)
  (mapcar #'parse-integer (str:split "," (first (get-file filename)))))

(defun parse-bingo-cards (filename)
  (let* ((input (cddr (get-file filename)))
         (n (length (str:words (first input))))
         card-rows
         bingo-cards)
    (setf card-rows (loop for line in input
                            for row = (mapcar #'parse-integer (str:words line))
                            if row collect row))
    (loop for row in card-rows
          for j = 0 then (mod (1+ j) n)
          with current-card = (make-array (list n n))
          do (loop for v in row
                   for i from 0
                   do
                   (setf (aref current-card j i) (list v nil))
                   (if (and (= (mod j n) (1- n)) (= i (1- n)))
                       (progn (push current-card bingo-cards)
                              (setf current-card (make-array (list n n)))))))
    bingo-cards))

(defun mark-card (number card)
  (loop with (n m) = (array-dimensions card)
        for j from 0 below n
        do
        (loop for i from 0 below m
                 for entry = (first (aref card j i))
                 if (= entry number)
                 do (setf (second (aref card j i)) t))))

(defun matrix-rows (matrix)
  (loop with (n m) = (array-dimensions matrix)
        for j from 0 below n
        collect (loop for i from 0 below m
                      collect (aref matrix j i))))

(defun matrix-columns (matrix)
  (loop with (n m) = (array-dimensions matrix)
        for j from 0 below n
        collect (loop for i from 0 below m
                      collect (aref matrix i j))))

(defun card-wins-p (card)
  (or (member t (mapcar (lambda (list) (every (lambda (e) (second e)) list))
                        (matrix-rows card)))
      (member t (mapcar (lambda (list) (every (lambda (e) (second e)) list))
                        (matrix-columns card)))))

(defun card-value (card)
  (loop for row in (matrix-rows card)
        sum (loop for entry in row
                  unless (second entry)
                  sum (first entry))))

(defun advent-04a (filename)
  (let ((numbers (parse-numbers filename))
        (bingo-cards (parse-bingo-cards filename))
        winning-card
        final-number)
    (loop for number in numbers
          do (loop for card in bingo-cards
                   do
                   (mark-card number card)
                   if (card-wins-p card)
                   (setf winning-card card
                         final-number number))
          until winning-card)
    (* (card-value winning-card) final-number)))

(defun advent-04b (filename)
  (let ((numbers (parse-numbers filename))
        (bingo-cards (parse-bingo-cards filename))
        last-winning-card
        remaining-numbers)
    (loop for number in numbers
          with remaining-cards = (reverse bingo-cards)
          do (loop for card in remaining-cards
                   do
                   (mark-card number card)
                   (setf remaining-numbers (rest numbers))
                   unless (card-wins-p card)
                   collect card into next-remaining-cards
                   finally (setf remaining-cards next-remaining-cards))
          until (= 1 (length remaining-cards))
          finally (setf last-winning-card (first remaining-cards)))

    (loop for number in remaining-numbers
          do (mark-card number last-winning-card)
          until (card-wins-p last-winning-card)
          finally (return (* (card-value last-winning-card) number)))))
