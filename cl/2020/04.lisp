;; ---------------------------- ;;
;; DAY 4 - PASSPORT PROCESSING  ;;
;; ---------------------------- ;;

(load "util.lisp")

(defun prepare-passports (input)
  (let ((file (get-file input))
        current-passport
        passports)
    ;; collect lines into passports
    (loop for line in file do
          (if (equal line "")
              (progn
                (push (loop for entry in current-passport
                            with cp
                            do
                            (let ((field (first (str:split ":" entry)))
                                  (value (second (str:split ":" entry))))
                              (push (list field value) cp))
                            finally (return cp))
                      passports)
                (setf current-passport '()))
              (setf current-passport (append (str:words line) current-passport))))
    passports))

(defun validate-height (height)
  (let ((n (length height))
        value units)
    (unless (< (length height) 3)
      (setf units (subseq height (- n 2))
            value (parse-integer (subseq height 0 (- n 2))))
      (cond ((equal units "cm")
             (within-range value 150 193))
            ((equal units "in")
             (within-range value 59 76))))))

(defun validate-entry (entry)
  (let ((field (first entry))
        (value (second entry)))

    ;; TODO: replace with read-from-string
    (cond ((equal field "byr")
           (within-range (parse-integer value) 1920 2002))
          ((equal field "iyr")
           (within-range (parse-integer value) 2010 2020))
          ((equal field "eyr")
           (within-range (parse-integer value) 2020 2030))
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
  (let ((passports (prepare-passports input)))
    (+ (count 'VALID (mapcar #'(lambda (p)
                                 (passport-type p (constantly T)))
                             passports))
       (count 'NPC (mapcar #'(lambda (p)
                               (passport-type p (constantly T)))
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

(defun day4 ()
  (format t "04: ~A, ~A~%" (day4a "input4.txt") (day4b "input4.txt")))
