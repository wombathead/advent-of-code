(ql:quickload :str)
(ql:quickload :cl-ppcre)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))

(defun get-contents (filename)
  " read FILENAME into a single string "
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun day1 ()
  (defun prepare-input (input)
    (sort (remove-duplicates (mapcar #'parse-integer (get-file input))) #'<))

  (defun day1a (input)
    " find the X and Y from input such that their sum is 2020 and return X*Y "
    (let ((input (prepare-input input))
          (numbers (make-hash-table))
          x y)

      (loop for i in input do
            (setf (gethash i numbers) (- 2020 i)))
      (loop for i in input do
            (if (gethash (- 2020 i) numbers)
                (return-from day1a (* i (- 2020 i)))))))

  (defun day1b (input)
    (let ((input (prepare-input input)))
      (loop for a in input and i from 0 do
            (loop for b in input and j from 0 do
                  (loop for c in input and k from 0 do
                        (unless (or (= i j) (= i k) (= j k))
                          (if (= 2020 (+ a b c))
                              (return-from day1b (* a b c)))))))))

  (format T "~A~%" (day1a "input1.txt"))
  (format T "~A~%" (day1b "input1.txt")))

(defun xor (a b)
  (and
    (or a b)
    (not (and a b))))

(defun day2 ()
  (defun get-passwords (input)
    " convert parameters into list of lists ((x y character password)...) "
    (let ((input (get-file input)))
      (mapcar #'(lambda (r)
                  ;; build list (x y character) password for each row
                  (list
                    (parse-integer (first (str:split "-" (first r))))
                    (parse-integer (second (str:split "-" (first r))))
                    (char (second r) 0)
                    (third r)))
              ;; split input on spaces to get each field
              (mapcar #'str:words input))))

  (defun valid-range (x y ch password)
    " return T if CH appears between X and Y times in PASSWORD "
    (let ((freq (count ch password)))
      (and (>= freq x) (<= freq y))))

  (defun valid-position (x y ch password)
    " return T if CH appears at position X xor position Y in PASSWORD "
    (xor (equal ch (char password (1- x)))
         (equal ch (char password (1- y)))))

  (defun day2a (input)
    (let ((passwords (get-passwords input)))
      (count T (mapcar #'(lambda (r) (apply #'valid-range r)) passwords))))

  (defun day2b (input)
    (let ((passwords (get-passwords input)))
      (count T (mapcar #'(lambda (r) (apply #'valid-position r)) passwords))))

  (format T "~A~%" (day2a "input2.txt"))
  (format T "~A~%" (day2b "input2.txt")))

(defun day3 ()
  (defun prepare-map (input)
    (let ((terrain (get-file input)))
      (make-array (list (length terrain) (length (first terrain)))
                  :initial-contents terrain)))

  (defun traverse (terrain dx dy)
    (let ((collisions 0)
          (i 0) (j 0)
          width height)
      (setf height (array-dimension terrain 0))
      (setf width (array-dimension terrain 1))
      (loop while (< j height) do
            (if (>= i width)
                (setf i (mod i width)))
            (if (equal (aref terrain j i) #\#)
                (incf collisions))
            (incf i dx)
            (incf j dy))
      collisions))

  (defun day3a (input)
    (let ((terrain (prepare-map input)))
      (traverse terrain 3 1)))

  (defun day3b (input)
    (let ((terrain (prepare-map input)))
      ;; calculate collisions for all slopes, and multiply together
      (apply #'* (loop for (dx dy) in '((1 1) (3 1) (5 1) (7 1) (1 2))
                       collect (traverse terrain dx dy)))))

  (format T "~A~%" (day3a "input3.txt"))
  (format T "~A~%" (day3b "input3.txt")))

(defun validate-range (num lower upper)
  (and (>= num lower) (<= num upper)))

(defun day4 ()
  (defun prepare-passports (input)
    (let ((file (get-file input))
          current-passport
          passports)
      ;; collect lines into passports
      (loop for line in file do
            (if (equal line "")
                (progn
                  (let (cp)
                    (loop for entry in current-passport do
                          (let ((field (first (str:split ":" entry)))
                                (value (second (str:split ":" entry))))
                            (push (list field value) cp)))
                    (push cp passports))
                  (setf current-passport '()))
                (setf current-passport (append (str:words line) current-passport))))
      passports))

  (defun validate-height (height)
    (let ((n (length height))
          value units)
      (unless (< (length height) 3)
        (setf units (subseq height (- n 2)))
        (setf value (parse-integer (subseq height 0 (- n 2))))
        (cond ((equal units "cm")
               (validate-range value 150 193))
              ((equal units "in")
               (validate-range value 59 76))))))

  (defun validate-entry (entry)
    (let ((field (first entry))
          (value (second entry)))
      (cond ((equal field "byr")
             (validate-range (parse-integer value) 1920 2002))
            ((equal field "iyr")
             (validate-range (parse-integer value) 2010 2020))
            ((equal field "eyr")
             (validate-range (parse-integer value) 2020 2030))
            ((equal field "hgt")
             (validate-height value))
            ((equal field "hcl")
             (and (equal #\# (char value 0))
                  (= (length (subseq value 1)) 6)
                  ;; TODO: better way to do this (with stl)
                  (every #'(lambda (c)
                             (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                         #\a #\b #\c #\d #\e #\f)))
                         (subseq value 1))))
            ((equal field "ecl")
             (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test 'equal))
            ((equal field "pid")
             (and (= (length value) 9) (every #'digit-char-p value)))
            ((equal field "cid")
             t))))

  (defun passport-type (passport validation-fn)
    (let ((fields-present (make-hash-table :test 'equal)))
      (loop for entry in passport do
            (if (funcall validation-fn entry)
                (setf (gethash (first entry) fields-present) T)))
      (let ((c (hash-table-count fields-present)))
        (cond ((= c 8)
               'VALID)
              ((and (= c 7) (equal (gethash "cid" fields-present) NIL))
               'NPC)
              (t
               'INVALID)))))

  (defun day4a (input)
    (let ((passports (prepare-passports input))
          (no-validation #'(lambda (e) t)))
      (+ (count 'VALID (mapcar #'(lambda (p)
                                   (passport-type p no-validation))
                               passports))
         (count 'NPC (mapcar #'(lambda (p)
                                 (passport-type p no-validation))
                             passports)))))

  (defun day4b (input)
    (let ((passports (prepare-passports input))
          (entry-validation #'(lambda (e) (validate-entry e))))
      (+ (count 'VALID (mapcar #'(lambda (p)
                                   (passport-type p entry-validation))
                               passports))
         (count 'NPC (mapcar #'(lambda (p)
                                 (passport-type p entry-validation))
                             passports)))))

  (format T "~A~%" (day4a "input4.txt"))
  (format T "~A~%" (day4b "input4.txt")))

(defun compute-row (row-str lower upper)
  (if (= (length row-str) 1)
      (if (eql #\F (char row-str 0))
          lower
          (1- upper))
      (if (eql #\F (char row-str 0))
          (compute-row (subseq row-str 2)
                       lower
                       (floor (+ upper lower) 2))
          (compute-row (subseq row-str 1)
                       (floor (+ upper lower) 2)
                       upper))))

(defun compute-col (col-str lower upper)
  (if (= (length col-str) 1)
      (if (eql #\L (char col-str 0))
          lower
          (1- upper))
      (if (eql #\L (char col-str 0))
          (compute-col (subseq col-str 1)
                       lower
                       (floor (+ upper lower) 2))
          (compute-col (subseq col-str 1)
                       (floor (+ upper lower) 2)
                       upper))))

(defun compute-seat-id (pass)
  " simply interpret PASS as a binary string of the seat ID: F,L=0; B,R=1 "
  (+ (* 8 (compute-row (subseq pass 0 7) 0 128))
     (compute-col (subseq pass 7) 0 8)))

(defun seat-id (pass)
  (let ((str pass))
    (setf str (cl-ppcre:regex-replace-all "F" str "0"))
    (setf str (cl-ppcre:regex-replace-all "B" str "1"))
    (setf str (cl-ppcre:regex-replace-all "L" str "0"))
    (setf str (cl-ppcre:regex-replace-all "R" str "1"))
    (parse-integer str :radix 2)))

(defun day5a (input)
  (let ((passes (get-file input)))
    (apply #'max (mapcar #'seat-id passes))))

(defun day5b (input)
  (let ((ids (sort (mapcar #'seat-id (get-file input)) #'<)))
    ;; if two IDs differ by 2 then I am between them
    (loop for (a b) on ids do
          (if (= (- b a) 2)
              (return-from day5b (1+ a))))))
