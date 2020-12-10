(ql:quickload :str)
(ql:quickload :cl-ppcre)

;; ---------------------------- ;;
;;  UTILITY/GENERAL FUNCTIONS   ;;
;; ---------------------------- ;;

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream NIL)
          while line
          collect line)))

(defun xor (a b)
  (and
    (or a b)
    (not (and a b))))

(defun within-range (num lower upper)
  " return T if NUM is between LOWER and UPPER "
  (and (>= num lower) (<= num upper)))

(defun breadth-first-search (s G)
  " perform BFS traversal on G starting from S "
  (let ((tree (make-hash-table :test 'equal))
        (discovered (make-hash-table :test 'equal))
        q)
    (setf (gethash s discovered) T)  ; mark s as discoverd
    (push s q)
    (loop while q do
          (let ((u (pop q)))
            ;; for each node v adjacent to u
            (loop for v in (gethash u G) do
                  (unless (gethash v discovered)
                    (setf (gethash v discovered) T) ; mark v discovered
                    (setf q (append q (list v)))    ; enqueue v
                    (push v (gethash u tree))))))   ; add edge in tree

    ;; return the tree and the size of the graph
    (values tree (hash-table-count discovered))))

(defun contiguous-sum (lst target)
  " find a contiguous sequence of items in LST that sum to TARGET "
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

;; ---------------------------- ;;
;;    DAY 1 - REPORT REPAIR     ;;
;; ---------------------------- ;;

(defun day1 ()
  (defun prepare-input (input)
    (sort (remove-duplicates (mapcar #'parse-integer (get-file input))) #'<))

  (defun day1a (input)
    " find the X and Y from input such that their sum is 2020 and return X*Y "
    (let ((input (prepare-input input))
          (numbers (make-hash-table)))

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

;; ---------------------------- ;;
;; DAY 2 - PASSWORD PHILOSOPHY  ;;
;; ---------------------------- ;;

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

;; ---------------------------- ;;
;; DAY 3 - TOBOGGAN TRAJECTORY  ;;
;; ---------------------------- ;;

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

;; ---------------------------- ;;
;; DAY 4 - PASSPORT PROCESSING  ;;
;; ---------------------------- ;;

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

;; ---------------------------- ;;
;;   DAY 5 - BINARY BOARDING    ;;
;; ---------------------------- ;;

(defun day5 ()
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
    (+ (* 8 (compute-row (subseq pass 0 7) 0 128))
       (compute-col (subseq pass 7) 0 8)))

  (defun seat-id (boarding-pass)
    " simply interpret PASS as a binary string of the seat ID: F,L=0; B,R=1 "
    (let ((str boarding-pass))
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

  (format T "~A~%" (day5a "input5.txt"))
  (format T "~A~%" (day5b "input5.txt")))

;; ---------------------------- ;;
;;    DAY 6 - CUSTOM CUSTOMS    ;;
;; ---------------------------- ;;

(defun day6 ()
  (defun prepare-answers (input)
    (let ((file (get-file input))
          (group-str "")
          (group-size 0)
          answers)
      (loop for line in file do
            (if (equal line "")
                (progn (push (list group-size group-str) answers)
                       (setf group-size 0)
                       (setf group-str ""))
                (progn (incf group-size)
                       (setf group-str
                             (concatenate 'string group-str line)))))
      answers))

  (defun day6a (input)
    (let ((answers (prepare-answers input)))
      (apply #'+ (mapcar
                   ;; count the number of distinct characters 
                   #'(lambda (a)
                       (length (remove-duplicates (second a))))
                   answers))))

  (defun day6b (input)
    (let ((answers (prepare-answers input)))
      (apply #'+ (mapcar
                   ;; count the number of times the frequency of a letter
                   ;; equals the group size
                   #'(lambda (a)
                       (let ((group-size (first a))
                             (answers (second a))
                             (sum 0))
                         (loop for question across (remove-duplicates answers) do
                               (if (= (count question answers) group-size)
                                   (incf sum)))
                         sum))
                   answers))))

  (format T "~A~%" (day6a "input6.txt"))
  (format T "~A~%" (day6b "input6.txt")))

;; ---------------------------- ;;
;;   DAY 7 - HANDY HAVERSACKS   ;;
;; ---------------------------- ;;

(defun day7 ()
  (defun get-contained-bags (str)
    " convert string \"[color] bag contains x [color] bags, ...\" into list (bag, quantity) of contained bags "
    (mapcar #'(lambda (x)
                (list (format NIL "~{~A~^ ~}" (subseq (str:words x) 1 3))
                      (parse-integer (first (str:words x)))))
            (cl-ppcre:all-matches-as-strings "\\d+( \\w+){2}" str)))

  (defun create-contains-graph (input)
    " create digraph where (u,v) in G iff bag u contains bag v "
    (let ((bags (get-file input))
          (edge-list (make-hash-table :test 'equal)))

      ;; for each line, add edge FROM contained bag TO containing bag
      (mapcar #'(lambda (l)
                  (let ((containing-bag (format NIL "~{~A~^ ~}"
                                                (subseq (str:words l) 0 2))))
                    (setf (gethash containing-bag edge-list)
                          (append (gethash containing-bag edge-list)
                                  (get-contained-bags l)))))
              bags)
      edge-list))

  (defun create-contained-by-graph (input)
    " create digraph where (u,v) in G iff bag u contains bag v "
    (let ((bags (get-file input))
          (edge-list (make-hash-table :test 'equal)))

      ;; for each line, add edge FROM contained bag TO containing bag
      (mapcar #'(lambda (l)
                  (let ((containing-bag (format NIL "~{~A~^ ~}"
                                                (subseq (str:words l) 0 2))))
                    (loop for bag in (get-contained-bags l) do
                          (push containing-bag
                                (gethash (first bag) edge-list)))))
              bags)
      edge-list))

  (defun contains (u G)
    " return the number of bags BAG contains in G "
    (if (gethash u G)
        ;; contains(u)= x(1+contains(v)) + y(1+contains(w)) ...
        (reduce #'+ (mapcar #'(lambda (v)
                                (let ((bag (first v))
                                      (weight (second v)))
                                  (* weight (1+ (contains bag G)))))
                            (gethash u G)))
        0))

  (defun day7a (input)
    (let ((G (create-contained-by-graph input))
          (target-bag "shiny gold")
          tree n)
      (setf (values tree n) (breadth-first-search target-bag G))
      (1- n)))

  (defun day7b (input)
    (let ((G (create-contains-graph input))
          (target-bag "shiny gold"))
      (contains target-bag G)))

  (format T "~D~%" (day7a "input7.txt"))
  (format T "~D~%" (day7b "input7.txt")))

;; ---------------------------- ;;
;;   DAY 8 - HANDHELD HALTING   ;;
;; ---------------------------- ;;

(defun day8 ()
  (defstruct instr
    op
    arg)

  (defun load-program (input)
    (let ((file (get-file input)))
      (mapcar #'(lambda (l)
                  ;; return a list (OP ARG VISITED)
                  (make-instr :op (read-from-string (first (str:words l)))
                              :arg (parse-integer (second (str:words l)))))
              file)))

  (defun execute-program (program)
    (let ((acc 0)
          (ip 0)
          (dirty (make-array (length program) :initial-element NIL)))

      (loop while (< ip (length program)) do
            (let ((instr (nth ip program))
                  (is-dirty (aref dirty ip))
                  op arg)
              (setf op (instr-op instr))
              (setf arg (instr-arg instr))

              (if is-dirty
                  (return-from execute-program (values acc 'LOOP))
                  (progn 
                    (setf (aref dirty ip) T)
                    (case op
                      (ACC (incf acc arg) (incf ip))
                      (JMP (incf ip arg))
                      (NOP (incf ip)))))))
      (values acc 'COMPLETE)))

  (defun determine-corruption (program)
    " determine the instruction such that flipping NOP to JMP (and vice versa) causes the PROGRAM to complete successfully "
    (loop for i from 0 below (length program) do
          (let ((p (mapcar #'copy-structure program))
                instr acc status)
            (setf instr (nth i p))
            (unless (eql (instr-op instr) 'ACC)
              (if (eql (instr-op instr) 'JMP)
                  (setf (instr-op (nth i p)) 'NOP)
                  (setf (instr-op (nth i p)) 'JMP))
              (setf (values acc status) (execute-program p))
              (if (eql status 'COMPLETE)
                  (return-from determine-corruption acc))))))

  (defun day8a (input)
    (let ((program (load-program input)))
      (execute-program program)))

  (defun day8b (input)
    (let ((program (load-program input)))
      (determine-corruption program)))

  (format T "~D~%" (day8a "input8.txt"))
  (format T "~D~%" (day8b "input8.txt")))

;; ---------------------------- ;;
;;    DAY 9 - ENCODING ERROR    ;;
;; ---------------------------- ;;

(defun day9 ()
  (defun is-valid? (num window)
    " determine whether NUM is the sum of any two elements in WINDOW "
    (loop for n in window and i from 0 do
          (loop for m in window and j from 0 do
                (unless (= i j)
                  (if (= (+ n m) num)
                      (return-from is-valid? T))))))

  (defun find-invalid (nums window-size)
    (loop for n in (subseq nums window-size) and i from 0 do
          (let ((window (subseq nums i (+ i window-size))))
            (if (not (is-valid? n window))
                (return-from find-invalid n)))))

  (defun day9a (input)
    (let ((nums (mapcar #'parse-integer (get-file input)))
          (window-size 25))
      (find-invalid nums window-size)))

  (defun day9b (input)
    (let ((nums (mapcar #'parse-integer (get-file input)))
          (window-size 25)
          contiguous)
      (setf contiguous
            (sort (contiguous-sum nums (find-invalid nums window-size)) #'<))
      (+ (first contiguous) (first (last contiguous)))))

  (format T "~D~%" (day9a "input9.txt"))
  (format T "~D~%" (day9b "input9.txt")))
