
(ql:quickload :str)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :parse-number)

(load "graph.lisp")
(load "vec2.lisp")

;; ---------------------------- ;;
;;  UTILITY/GENERAL FUNCTIONS   ;;
;; ---------------------------- ;;

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
  " return T if NUM is between LOWER and UPPER "
  (and (>= num lower) (<= num upper)))

(defun within-all-ranges (num ranges)
  (not (member NIL (mapcar #'(lambda (r) (within-range num (first r) (second r)))
                           ranges))))

(defun within-some-range (num ranges)
  (member t (mapcar #'(lambda (r) (within-range num (first r) (second r)))
                    ranges)))

(defun binary-to-dec (bin-str zero-char lo hi)
  " convert binary string BIN-STR with ZERO-CHAR representing 0 to decimal value "
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

  (format t "1: ~A, ~A~%" (day1a "input1.txt") (day1b "input1.txt")))

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
    (alexandria:xor (equal ch (char password (1- x)))
                    (equal ch (char password (1- y)))))

  (defun day2a (input)
    (let ((passwords (get-passwords input)))
      (count T (mapcar #'(lambda (r) (apply #'valid-range r)) passwords))))

  (defun day2b (input)
    (let ((passwords (get-passwords input)))
      (count T (mapcar #'(lambda (r) (apply #'valid-position r)) passwords))))

  (format t "~A~%" (day2a "input2.txt"))
  (format t "~A~%" (day2b "input2.txt")))

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
      (setf 
        height (array-dimension terrain 0)
        width (array-dimension terrain 1))
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

  (format t "~A~%" (day3a "input3.txt"))
  (format t "~A~%" (day3b "input3.txt")))

;; ---------------------------- ;;
;; DAY 4 - PASSPORT PROCESSING  ;;
;; ---------------------------- ;;

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
  (format t "~A~%" (day4a "input4.txt"))
  (format t "~A~%" (day4b "input4.txt")))

;; ---------------------------- ;;
;;   DAY 5 - BINARY BOARDING    ;;
;; ---------------------------- ;;

(defun day5 ()
  (defun compute-row (row-str)
    (binary-to-dec row-str #\F 0 (expt 2 (length row-str))))

  (defun compute-col (col-str)
    (binary-to-dec col-str #\L 0 (expt 2 (length col-str))))

  (defun compute-seat-id (pass)
    (+ (* 8 (compute-row (subseq pass 0 7)))
       (compute-col (subseq pass 7))))

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

  (format t "~A~%" (day5a "input5.txt"))
  (format t "~A~%" (day5b "input5.txt")))

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
                       (setf group-size 0
                             group-str ""))
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

  (format t "~A~%" (day6a "input6.txt"))
  (format t "~A~%" (day6b "input6.txt")))

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
      (setf (values tree n) (graph:breadth-first-search target-bag G))
      (1- n)))

  (defun day7b (input)
    (let ((G (create-contains-graph input))
          (target-bag "shiny gold"))
      (contains target-bag G)))

  (format t "~D~%" (day7a "input7.txt"))
  (format t "~D~%" (day7b "input7.txt")))

;; ---------------------------- ;;
;;   DAY 8 - HANDHELD HALTING   ;;
;; ---------------------------- ;;

(defstruct instr
  op
  arg)

(defun day8 ()
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

              (setf op (instr-op instr)
                    arg (instr-arg instr))

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
    " determine the instruction such that flipping NOP to JMP (or vice versa) causes PROGRAM to complete successfully (to the final instruction) "
    (loop for i from 0 below (length program) do
          ;; DEEP COPY means we can modify P without changing PROGRAM
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

  (format t "~D~%" (day8a "input8.txt"))
  (format t "~D~%" (day8b "input8.txt")))

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

  (format t "~D~%" (day9a "input9.txt"))
  (format t "~D~%" (day9b "input9.txt")))

;; ---------------------------- ;;
;;    DAY 10 - ADAPTER ARRAY    ;;
;; ---------------------------- ;;

(defun make-joltage-graph (joltages)
  (let ((G (make-hash-table :test 'equal)))
    (loop for u in joltages and i from 0 do
          (loop for v in (subseq joltages (1+ i)) do
                (if (<= (- v u) 3)
                    (push v (gethash u G))
                    (return))))
    G))

(defun day10a (input)
  (let ((joltages (mapcar #'parse-integer (get-file input)))
        differences)

    ;; for the adapter
    (push 0 joltages)
    ;; for your device
    (push (+ 3 (reduce #'max joltages)) joltages)

    (setf differences (loop for (a b) on (sort joltages #'<)
                            collect (unless (eql b NIL)
                                      (- b a))))
    (* (count 1 differences) (count 3 differences))))

(defun day10b (input)
  (let ((joltages (mapcar #'parse-integer (get-file input)))
        (G (make-hash-table :test 'equal))
        max-adapter)

    (push 0 joltages)
    (setf max-adapter (+ 3 (reduce #'max joltages)))
    (push max-adapter joltages)
    (setf joltages (sort joltages #'<))

    (setf G (make-joltage-graph joltages))
    (graph:count-paths G 0 max-adapter)))

(defun day10 ()
  (format t "~D~%" (day10a "input10.txt"))
  (format t "~D~%" (day10b "input10.txt")))

;; ---------------------------- ;;
;;   DAY 11 - SEATING SYSTEM    ;;
;; ---------------------------- ;;

(defun seat-occupied? (ch)
  (char= ch #\#))

(defun seat-empty? (ch)
  (char= ch #\L))

(defun count-occupied-seats (grid)
  (let ((occupied 0))
    (loop for j from 0 below (first (array-dimensions grid)) do
          (loop for i from 0 below (second (array-dimensions grid)) do
                (if (char= (aref grid j i) #\#)
                    (incf occupied))))
    occupied))

(defun adjacent-occupied-seats (grid y x)
  (let ((dimensions (array-dimensions grid))
        (occupied 0)
        width height)

    (setf height (first dimensions)
          width (second dimensions))

    ;; TODO: good candidate for macro?
    (if (and (plusp y) (plusp x))
        (if (seat-occupied? (aref grid (1- y) (1- x)))
            (incf occupied)))
    (if (plusp y)
        (if (seat-occupied? (aref grid (1- y) x))
            (incf occupied)))
    (if (and (plusp y) (< x (1- width)))
        (if (seat-occupied? (aref grid (1- y) (1+ x)))
            (incf occupied)))

    (if (plusp x)
        (if (seat-occupied? (aref grid y (1- x)))
            (incf occupied)))
    (if (< x (1- width))
        (if (seat-occupied? (aref grid y (1+ x)))
            (incf occupied)))
    
    (if (and (< y (1- height)) (plusp x))
        (if (seat-occupied? (aref grid (1+ y) (1- x)))
            (incf occupied)))
    (if (and (< y (1- height)))
        (if (seat-occupied? (aref grid (1+ y) x))
            (incf occupied)))
    (if (and (< y (1- height)) (< x (1- width)))
        (if (seat-occupied? (aref grid (1+ y) (1+ x)))
            (incf occupied)))
    
    occupied))

(defun visible-occupied-seat (grid y x dy dx)
  (let ((height (first (array-dimensions grid)))
        (width (second (array-dimensions grid))))
    (loop while T do
          (incf y dy)
          (incf x dx)
          (if (or (minusp y) (> y (1- height))
                  (minusp x) (> x (1- width)))
              (return)

              (case (aref grid y x)
                (#\# (return-from visible-occupied-seat (list y x)))
                (#\L (return)))))
    NIL))

(defun visible-occupied-seats (grid y x)
   (let ((occupied 0)
        (directions '((-1 0)    ; up
                      (-1 1)    ; up right
                      (0 1)     ; right
                      (1 1)     ; down right
                      (1 0)     ; down
                      (1 -1)    ; down left
                      (0 -1)    ; left
                      (-1 -1))))
     (loop for (dy dx) in directions do
           (let ((visible-seat (visible-occupied-seat grid y x dy dx)))
             (if visible-seat
                 (progn
                   (incf occupied)))))
     occupied))

(defun step-automaton (grid)
  ;; TODO: implement with Wolfram code as argument?
  (let ((new-grid (alexandria:copy-array grid))
        (height (first (array-dimensions grid)))
        (width (second (array-dimensions grid)))
        (changed? NIL))
    (loop for j from 0 below height do
          (loop for i from 0 below width do
                (case (aref grid j i)
                  (#\L
                   (if (= (adjacent-occupied-seats grid j i) 0)
                       (setf (aref new-grid j i) #\#
                             changed? T)))
                  (#\#
                   (if (>= (adjacent-occupied-seats grid j i) 4)
                       (setf (aref new-grid j i) #\L
                             changed? T))))))
    (values new-grid changed?)))

(defun step-automaton-visual (grid)
  ;; TODO: implement with Wolfram code as argument?
  (let ((new-grid (alexandria:copy-array grid))
        (height (first (array-dimensions grid)))
        (width (second (array-dimensions grid)))
        (changed? NIL))

    (loop for j from 0 below height do
          (loop for i from 0 below width do
                (case (aref grid j i)
                  (#\L
                   (if (= (visible-occupied-seats grid j i) 0)
                       (setf (aref new-grid j i) #\#
                             changed? T)))
                  (#\#
                   (if (>= (visible-occupied-seats grid j i) 5)
                       (setf (aref new-grid j i) #\L
                             changed? T))))))
    (values new-grid changed?)))

(defun grids-equal? (a b)
  " return true if 2D arrays A and B are equal in every position "
  (loop for j from 0 below (first (array-dimensions a)) do
        (loop for i from 0 below (second (array-dimensions a)) do
              (unless (equal (aref a j i) (aref b j i))
                (return-from grids-equal? NIL))))
  T)

(defun make-grid (input)
  (let ((file (get-file input)))
    (make-array (list (length file) (length (first file)))
                :initial-contents file)))

(defun day11a (input)
  (let ((grid (make-grid input))
        (grid-changed T))

    (loop while grid-changed do
          (setf (values grid grid-changed) (step-automaton grid)))

    (count-occupied-seats grid)))

(defun day11b (input)
  (let ((grid (make-grid input))
        (grid-changed T))

    (loop while grid-changed do
          (setf (values grid grid-changed) (step-automaton-visual grid)))

    (count-occupied-seats grid)))

(defun day11 ()
  (format t "~D~%" (day11a "input11.txt"))
  (format t "~D~%" (day11b "input11.txt")))

;; ---------------------------- ;;
;;      DAY 12 - RAIN RISK      ;;
;; ---------------------------- ;;

(defun get-instructions (file)
  (let ((input (get-file file)))
    (mapcar #'(lambda (l)
                (list (read-from-string (subseq l 0 1))
                      (read-from-string (subseq l 1))))
            input)))

(defstruct ship
  (pos (vec2:make-vec2) :type vec2:vec2)
  (view (vec2:make-vec2 :x 1 :y 0) :type vec2:vec2))

(defmacro ship-pos-x (ship) `(vec2:vec2-x (ship-pos ,ship)))
(defmacro ship-pos-y (ship) `(vec2:vec2-y (ship-pos ,ship)))
(defmacro ship-view-x (ship) `(vec2:vec2-x (ship-view ,ship)))
(defmacro ship-view-y (ship) `(vec2:vec2-y (ship-view ,ship)))

(defun move-ship (ship instruction)
  (let ((dir (first instruction))
        (dst (second instruction))
        ;; TODO: perform deep copy here
        (s (copy-ship ship)))
    (case dir
      (N (incf (ship-pos-y s) dst))
      (S (decf (ship-pos-y s) dst))
      (E (incf (ship-pos-x s) dst))
      (W (decf (ship-pos-x s) dst))

      (F (incf (ship-pos-x s)
               (* dst (ship-view-x s)))
         (incf (ship-pos-y s)
               (* dst (ship-view-y s))))

      (L (setf (ship-view s)
               (case dst
                 (90 (vec2:rotate-90 (ship-view s)))
                 (180 (vec2:rotate-90 (vec2:rotate-90 (ship-view s))))
                 (270 (vec2:rotate-270 (ship-view s)))
                 (t (ship-view s)))))

      ;; TODO: handle more elegantly (but still accurately)
      (R (setf (ship-view s)
               (case dst
                 (90 (vec2:rotate-270 (ship-view s)))
                 (180 (vec2:rotate-90 (vec2:rotate-90 (ship-view s))))
                 (270 (vec2:rotate-90 (ship-view s)))
                 (t (ship-view s))))))
    s))

(defun move-waypoint (s w instruction)
  (let ((dir (first instruction))
        (dst (second instruction)))
    (case dir
      (N (incf (vec2:vec2-y w) dst))
      (S (decf (vec2:vec2-y w) dst))
      (E (incf (vec2:vec2-x w) dst))
      (W (decf (vec2:vec2-x w) dst))

      (F 
        (let ((diff (vec2:sub w s)))
          (setf s (vec2:add s (vec2:mult dst diff))
                w (vec2:add s diff))))

      (L
        (setf w
              (case dst
                (90 (vec2:rotate-90-about w s))
                (180 (vec2:rotate-90-about (vec2:rotate-90-about w s)
                                           s))
                (270 (vec2:rotate-270-about w s)))))
      (R
        (setf w
              (case dst
                (90 (vec2:rotate-270-about w s))
                (180 (vec2:rotate-90-about (vec2:rotate-90-about w s)
                                           s))
                (270 (vec2:rotate-90-about w s))))))
    (values s w)))

(defun day12a (input)
  (let ((instructions (get-instructions input))
        (ship (make-ship)))
    (loop for instr in instructions do
          (setf ship (move-ship ship instr)))

    ;; return the Manhattan distance of the ship from the origin
    (vec2:l1 (ship-pos ship) (vec2:make-vec2 :x 0 :y 0))))

(defun day12b (input)
  (let ((instructions (get-instructions input))
        (ship (vec2:make-vec2 :x 0 :y 0))
        (waypoint (vec2:make-vec2 :x 10 :y 1))
        (origin (vec2:make-vec2)))
    (loop for instr in instructions do
          (setf (values ship waypoint) (move-waypoint ship waypoint instr)))
    (vec2:l1 ship origin)))

(defun day12 ()
  (format t "~D~%" (day12a "input12.txt"))
  (format t "~D~%" (day12b "input12.txt")))

;; ---------------------------- ;;
;;   DAY 13: SHUTTLE SEARCH     ;;
;; ---------------------------- ;;

(defun get-buses (file)
  (let ((input (get-file file))
        earliest-time
        buses)

    (setf earliest-time (parse-integer (first input))
          buses (str:split "," (second input)))

    (list earliest-time buses)))

(defun earliest-arrival (minimum bus)
  (let (q r)
    (setf (values q r) (floor minimum bus))
    (+ minimum (- bus r))))

(defun day13a (file)
  (let ((input (get-buses file))
        minimum
        buses
        earliest-time
        earliest-bus)

    (setf minimum (first input)
          buses (mapcar #'parse-integer
                        (remove-if #'(lambda (b)
                                       (equal b "x"))
                                   (second input)))
          earliest-time (apply #'min (mapcar #'(lambda (b)
                                                 (earliest-arrival minimum b))
                                             buses)))
    (loop for bus in buses do
          (if (= (mod earliest-time bus) 0)
              (setf earliest-bus bus)))
    (* earliest-bus (- earliest-time minimum))))

(defun gcd++ (a b)
  " return the x and y such that ax + by = gcd(a,b) using the Extended Euclidean Algorithm "
  (let ((r- a) (r b)
        (s- 1) (s 0)
        (u- 0) (u 1)
        q)
    
    (loop while (/= r 0) do
          (setf q (floor r- r)
                (values r- r) (values r (- r- (* q r)))
                (values s- s) (values s (- s- (* q s)))  
                (values u- u) (values u (- u- (* q u)))))

    (values s- u-)))

(defun crt (congruences)
  " given x = a1 (mod n1) = a2 (mod n2) ... supplied in the list ((a1 n1) ...) CONGRUENCES, compute x using the Chinese Remainder Theorem "
  (loop while (> (length congruences) 1) do
        (let ((d1 (pop congruences))
              (d2 (pop congruences))
              a1 m1 n1
              a2 m2 n2)

          (setf a1 (first d1)
                n1 (second d1)  
                a2 (first d2)  
                n2 (second d2))

          (setf (values m1 m2) (gcd++ n1 n2))

          (let ((a3 (+ (* a1 m2 n2) (* a2 m1 n1)))
                (n3 (* n1 n2))
                k)

            (when (minusp a3)
                (setf k (ceiling (- a3) n3))
                (incf a3 (* k n3)))
            
            (push (list a3 n3) congruences))))

  (values (caar congruences) (cadar congruences)))

(defun day13b (file)
  (let ((input (str:split "," (second (get-file file))))
        congruences a n)
    (setf input (mapcar #'read-from-string
                        input))

    (setf congruences (loop for n in input and a from 0
                            unless (eql n 'X)
                            collect (list (- n a) n)))
    (setf (values a n) (crt congruences))
    (mod a n)))

(defun day13 ()
  (format t "~D~%" (day13a "input13.txt"))
  (format t "~D~%" (day13b "input13.txt")))

;; ---------------------------- ;;
;;     DAY 14: DOCKING DATA     ;;
;; ---------------------------- ;;

(defstruct mask-write
  mask
  writes)

(defun empty-string (str)
  (equal str ""))

(defun starts-with (regex target-str)
  (let ((idx (cl-ppcre:all-matches regex target-str)))
    (unless (null idx)
      (zerop (first idx)))))

(defun masked-value (mask num)
  ;; OR mask: marks positions we want a 1
  ;; AND mask: marks positions we want a 0
  (let ((or-mask (parse-integer (cl-ppcre:regex-replace-all "X" mask "0")
                                :radix 2))
        (and-mask (parse-integer (cl-ppcre:regex-replace-all "X" mask "1")
                                 :radix 2)))
    (logand (logior num or-mask) and-mask)))

(defun get-writes (file)
  " return list ((mask ((address value) ...)) ... ) of memory writes "
  (let ((input (get-file file))
        instructions
        (curr (make-mask-write)))
    (loop for line in input do
          (if (empty-string line)
              (progn 
                (setf (mask-write-writes curr) (reverse (mask-write-writes curr)))
                (push curr instructions)
                (setf curr (make-mask-write))
                (return)))

          (if (starts-with "mask" line)
              ;; push the previous sequence of writes
              (progn 
                (setf (mask-write-writes curr) (reverse (mask-write-writes curr)))
                (push curr instructions)
                (setf curr (make-mask-write)
                      (mask-write-mask curr) (third (str:words line))))

              ;; add the write to the current sequence
              (let ((value (parse-integer
                             (third (str:words line))))
                    (idx (cl-ppcre:all-matches "\[[0-9]+\]" line))
                    lo hi address)

                (setf lo (1+ (first idx))
                      hi (1- (second idx))
                      address (parse-integer (subseq line lo hi)))

                (push (list address value) (mask-write-writes curr)))))
    (setf instructions (reverse instructions))))

(defun floating-masked-value (mask num)
  (let ((num-str (format nil "~36,'0B" num))
        result)
    (setf result (copy-seq num-str))
    (loop for bit across mask and i from 0 do
          (setf (char result i) (case bit
                                  (#\0 (char num-str i))
                                  (otherwise bit)))
          finally (return result))))

(defun generate-combinations (str)
  (let ((n (length str))
        (possible (if (char= (char str 0) #\X)
                      (list "0" "1")
                      (list (subseq str 0 1)))))
    (if (= n 1)
        (return-from generate-combinations possible)

        (alexandria:map-product
          #'(lambda (x y)
              (concatenate 'string x y))
          possible
          (generate-combinations (subseq str 1))))))

(defun day14a (file)
  (let ((writes (get-writes file))
        (memory (make-hash-table)))
    (loop for w in writes do
          (let ((mask (mask-write-mask w)))
            (loop for mem-write in (mask-write-writes w) do
                  (setf (gethash (first mem-write) memory)
                        (masked-value mask (second mem-write))))))
    (let ((sum 0))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (incf sum v))
               memory)
      sum)))

(defun day14b (file)
  (let ((writes (get-writes file))
        (memory (make-hash-table)))
    
    (loop for w in writes do
          (let ((mask (mask-write-mask w)))
            (loop for mem-write in (mask-write-writes w) do
                  (let ((address (first mem-write))
                        (value (second mem-write))
                        locations)
                    (setf locations (generate-combinations
                                      (floating-masked-value mask address)))
                    (loop for location in (mapcar #'(lambda (x)
                                                      (parse-integer x :radix 2))
                                                  locations) do
                          (setf (gethash location memory) value))))))
    
    (let ((sum 0))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (incf sum v))
               memory)
      sum)))

(defun day14 ()
  (format t "~D~%" (day14a "input14.txt"))
  (format t "~D~%" (day14b "input14.txt")))

;; --------------------------------- ;;
;;  DAY 15: RAMBUNCTIOUS RECITATION  ;;
;; --------------------------------- ;;

(defun elf-game (starting-numbers max-round)
  " run the elf game up to round MAX-ROUND with STARTING-NUMBERS as input, and return the final spoken number "
  ;; TODO: compare speed with array implementation?
  (let ((utterances (make-hash-table))
        (last-seen (make-hash-table))
        say
        prev)
    (loop for n in starting-numbers and i from 1 do
          (setf (gethash n utterances) 1
                (gethash n last-seen) (list i)
                prev n))
    
    (loop for i from (1+ (length starting-numbers)) to max-round do
          (setf say (if (= 1 (gethash prev utterances))
                        0
                        (- (first (gethash prev last-seen))
                           (second (gethash prev last-seen)))))

          (if (gethash say utterances)
              (progn
                (incf (gethash say utterances))
                (setf (gethash say last-seen) (list i (first (gethash say last-seen)))))

              (progn
                (setf (gethash say utterances) 1
                      (gethash say last-seen) (list i))))

          (setf prev say))
    prev))

(defun day15a (file)
  (let ((input (mapcar #'parse-integer
                       (str:split "," (first (get-file file))))))
    (elf-game input 2020)))

(defun day15b (file)
  (let ((input (mapcar #'parse-integer
                       (str:split "," (first (get-file file))))))
    (elf-game input 30000000)))

(defun day15 ()
  (format t "~D~%" (day15a "input15.txt"))
  (format t "~D~%" (day15b "input15.txt")))

;; -------------------------------- ;;
;;    DAY 16: TICKET TRANSLATION    ;;
;; -------------------------------- ;;

(defun parse-input (file)
  " returns three values: 1) list ((field ((lo1 hi1) (lo2 hi2) ...)) ...) of fields and ranges; 2) list (n1 n2 ...) of values on my ticket; 3) list ((n11 n12 ...) (n21 n22 ...) ...) of nearby ticket values "
  (let ((input (get-file file))
        fields
        my-ticket
        nearby-tickets)

    ;; loop to gather fields and ranges
    (loop for line in input and i from 0 do
          (setf input (rest input))

          (if (string= line "")
              (return))

          (let ((field-name (first (str:split ":" line)))
                (ranges (mapcar #'parse-integer
                                (cl-ppcre:all-matches-as-strings "\\d+" line))))
            (push (list field-name
                        (loop :for (a b) :on ranges :by #'cddr :while b
                              :collect (list a b)))
                  fields)))

    ;; loop to gather my ticket information
    (loop for line in input do
          (setf input (rest input))
          (unless (starts-with "your" line)
            (if (string= "" line)
                (return)
                (setf my-ticket (mapcar #'parse-integer
                                        (str:split "," line))))))

    ;; loop to gather nearby ticket information
    (loop for line in input do
          (unless (starts-with "nearby" line)
            (push (mapcar #'parse-integer (str:split "," line))
                  nearby-tickets)))

    (values fields my-ticket nearby-tickets)))

(defun invalid-ticket-values (field-ranges ticket)
  (let ((ranges (alexandria:flatten (mapcar #'cadr field-ranges)))
        invalid-values)

    (loop for value in ticket do
          (let ((never-valid t))
            (loop :for (a b) :on ranges :by #'cddr :while b do
                  (when (within-range value a b)
                    (setf never-valid nil)
                    (return)))

            (if never-valid
                (push value invalid-values))))
    invalid-values))

(defun day16a (file)
  (let (field-ranges my-ticket nearby-tickets)
    (setf (values field-ranges my-ticket nearby-tickets)
          (parse-input file))

    ;; this seems to perform slightly better than summing as you go
    (reduce #'+ (alexandria:flatten
                  (mapcar #'(lambda (tkt) (invalid-ticket-values field-ranges tkt))
                          nearby-tickets)))))

(defun make-matching-graph (field-ranges valid-tickets)
  (let ((n (length field-ranges))
        (G (make-hash-table))

        ;; specifies one list of ranges per field
        (ranges (mapcar #'second field-ranges)))

    (loop for i from 0 below n do
          ;; get all first, second, ..., i-th values on nearby tickets
          (let ((ith-values (mapcar #'(lambda (tkt) (elt tkt i))
                                    valid-tickets)))

            (loop for range-list in ranges and j from n do

                  ;; add edge when all ith values are in some range for this field
                  (when (not (member NIL (mapcar #'(lambda (v) (within-some-range v range-list))
                                                 ith-values)))

                    ;; e = (node capacity flow)
                    (push (list j 1 0) (gethash i G))))))
    
    (loop for i from 0 below n do
          ;; add edge from source to all left hand nodes
          (push (list i 1 0) (gethash 'source G)) 
          ;; add edge from all right hand nodes to sink
          (push (list 'sink 1 0) (gethash (+ i n) G)))
    
    G))

(defun print-graph (G)
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) G))

(defun copy-graph (G)
  (let ((H (make-hash-table)))
    (maphash #'(lambda (k v)
                 (setf (gethash k H) (copy-list v)))
             G)
    H))

(defun augmenting-path (G source sink)
  (let ((pred (make-hash-table))
        q)

    (push source q)

    ;; find the augmenting path
    (loop while q do
          (let ((u (pop q)))
            (loop for e in (gethash u G) do
                  (let ((v (first e))
                        (cap (second e))
                        (flow (third e)))

                    (when (and (not (gethash v pred))
                               (> cap flow))
                      (setf (gethash v pred) (list u e))
                      (push v q))))))

    ;; return path if there is path from source to sink
    (if (gethash sink pred)
        pred)))

(defun augment-flow (G augmenting-path source sink)
  (let ((v sink)
        (H (copy-graph G))
        df)

    ;; compute maximum amount by which to increase flow
    (loop while (gethash v augmenting-path) do
          (let ((parent (first (gethash v augmenting-path)))
                (cap (second (second (gethash v augmenting-path))))
                (flow (third (second (gethash v augmenting-path)))))

            (setf df (if df
                         (min df (- cap flow))
                         (- cap flow)))

            (setf v parent)
            (if (eql parent source)
                (return))))

    ;; now send flow along the edges
    (setf v sink)
    (loop while (gethash v augmenting-path) do
          (let ((incident-edges
                  (gethash (first (gethash v augmenting-path)) H)))

            ;; find the edge which was taken
            (loop for e in incident-edges do
                  (when (eql (first e) v)
                    (incf (third e) df))))

          ;; move to parent node
          (setf v (first (gethash v augmenting-path))))
    H))

(defun edmonds-karp (G source sink)
  (let ((H (copy-graph G)))
    
    (loop while (augmenting-path H source sink) do
          (let ((path (augmenting-path H source sink)))
            (setf H (augment-flow H path source sink))))
    H))

(defun matching-to-fields (M field-ranges)
  (let ((n (length field-ranges))
        (field-names (mapcar #'first field-ranges))
        (H (copy-graph M))
        fields)

    (loop for i from 0 below n do
          (loop for e in (gethash i M) do
                (if (= (third e) 1)
                    (setf (gethash i H) (first e)))))

    (loop for i from 0 below n do
          (format t "~D: ~A~%" i (gethash i H))
          (let ((field-index (mod (gethash i H) n)))
            (push (list i (nth field-index field-names)) fields)))

    (reverse fields)))

(defun day16b (file)
  (let (field-ranges
        my-ticket
        nearby-tickets
        valid-tickets
        n
        possible-fields
        ticket-values
        matching)

    (setf (values field-ranges my-ticket nearby-tickets)
          (parse-input file))
    
    (setf valid-tickets
          (remove-if #'(lambda (tkt) (invalid-ticket-values field-ranges tkt))
                     nearby-tickets))
    
    (setf possible-fields (make-array (list (length my-ticket))
                                      :initial-element nil))
    
    (setf n (length my-ticket))

    ;; ticket-values[0] = all ticket values at position 0 (i.e. appearing first)
    (setf ticket-values (loop for i from 0 below n
                             collect (mapcar #'(lambda (tkt) (nth i tkt))
                                             valid-tickets)))
    
    (loop for position in ticket-values and i from 0 do
          (loop for ranges in (mapcar #'second field-ranges) and j from 0 do
                (let ((all-in t))
                  (loop for value in position do
                        (if (not (within-some-range value ranges))
                            (setf all-in nil)))
                  (when all-in
                      (push j (aref possible-fields i))))))

    (loop while t do
          (let (matched-position
                matched-field
                (found nil))
            
            (loop for i from 0 below n do
                  (when (= 1 (length (aref possible-fields i)))

                    (setf matched-position i
                          matched-field (first (aref possible-fields i))
                          found t)

                    ;; add the pair to the matching
                    (push (list matched-position matched-field) matching)

                    ;; remove the matched-field from each other
                    (loop for j from 0 below n do
                          (setf (aref possible-fields j)
                                (remove matched-field (aref possible-fields j))))))
            
            (if (not found)
                (return))))
    
    (let ((result 1))
      (loop for e in matching do
            (let ((pos (first e))
                  (field (second e)))
              (if (starts-with "departure" 
                               (first (nth field field-ranges)))
                  (setf result (* result (nth pos my-ticket))))))
      result)))

(defun day16 ()
  (format t "~D~%" (day16a "input16.txt"))
  (format t "~D~%" (day16b "input16.txt")))

;; ---------------------------- ;;
;;     DAY 17: CONWAY CUBES     ;;
;; ---------------------------- ;;

(defun alivep (cell grid)
  (member cell (alexandria:hash-table-keys grid) :test (hash-table-test grid)))

(defun step-3d-automaton (grid)
  (let ((new-grid (make-hash-table :test (hash-table-test grid)))
        (neighbour-counts (make-hash-table :test (hash-table-test grid))))
    
    ;; count neighbours for neighbourhood of each active cell
    (loop for key in (alexandria:hash-table-keys grid)
          for value = (gethash key grid) do
          (let ((x (first key))
                (y (second key))
                (z (third key)))

            (loop for k from -1 to 1 do
                  (loop for j from -1 to 1 do
                        (loop for i from -1 to 1 do
                              (let ((cell (list (+ x i)
                                                (+ y j)
                                                (+ z k))))
                                (unless (and (zerop i) (zerop j) (zerop k))
                                  (if (gethash cell neighbour-counts)
                                      (incf (gethash cell neighbour-counts))
                                      (setf (gethash cell neighbour-counts) 1)))))))))

    (loop for cell in (alexandria:hash-table-keys neighbour-counts)
          for neighbours = (gethash cell neighbour-counts) do

          (if (alivep cell grid)
              ;; if active, remain active if it has 2 or 3 active neighbours
              (if (or (= neighbours 2) (= neighbours 3))
                  (setf (gethash cell new-grid) t))

              ;; otherwise become active if exactly 3 neighbours are
              (if (= neighbours 3)
                  (setf (gethash cell new-grid) t))))

    new-grid))

;; TODO: handle 4d automaton more gracefully (MACROS)
(defun step-4d-automaton (grid)
  (let ((new-grid (make-hash-table :test (hash-table-test grid)))
        (neighbour-counts (make-hash-table :test (hash-table-test grid))))
    
    ;; count neighbours for neighbourhood of each active cell
    (loop for key in (alexandria:hash-table-keys grid)
          for value = (gethash key grid) do
          (let ((x (first key))
                (y (second key))
                (z (third key))
                (w (fourth key)))

            (loop for l from -1 to 1 do
                  (loop for k from -1 to 1 do
                        (loop for j from -1 to 1 do
                              (loop for i from -1 to 1 do
                                    (let ((cell (list (+ x i)
                                                      (+ y j)
                                                      (+ z k)
                                                      (+ w l))))
                                      (unless (and (zerop i) (zerop j)
                                                   (zerop k) (zerop l))
                                        (if (gethash cell neighbour-counts)
                                            (incf (gethash cell neighbour-counts))
                                            (setf (gethash cell neighbour-counts) 1))))))))))

    (loop for cell in (alexandria:hash-table-keys neighbour-counts)
          for neighbours = (gethash cell neighbour-counts) do

          (if (alivep cell grid)
              ;; if active, remain active if it has 2 or 3 active neighbours
              (if (or (= neighbours 2) (= neighbours 3))
                  (setf (gethash cell new-grid) t))

              ;; otherwise become active if exactly 3 neighbours are
              (if (= neighbours 3)
                  (setf (gethash cell new-grid) t))))

    new-grid))

(defun print-neighbour-counts (g)
  (maphash #'(lambda (k v) (format t "~A: ~D~%" k v)) g))

(defun print-grid (g)
  (let ((key-test (hash-table-test g))
        (xs (mapcar #'first (alexandria:hash-table-keys g)))
        (ys (mapcar #'second (alexandria:hash-table-keys g)))
        (zs (mapcar #'third (alexandria:hash-table-keys g)))
        min-x max-x
        min-y max-y
        min-z max-z)

    (setf min-x (apply #'min xs)
          max-x (apply #'max xs)
          min-y (apply #'min ys)
          max-y (apply #'max ys)
          min-z (apply #'min zs)
          max-z (apply #'max zs))

    (loop for z from min-z to max-z do
          (format t "z=~D~%" z)
          (loop for y from min-y to max-y do
                (loop for x from min-x to max-x do
                      (princ
                        (if (member (list x y z)
                                    (alexandria:hash-table-keys g) :test key-test)
                            "#"
                            ".")))
                (terpri))
          (terpri))))

(defun day17a (file)
  (let ((input (get-file file))
        (active-cells (make-hash-table :test #'equal))
        (timesteps 6))

    (loop for row in input and j from 0 do
          (loop for c across row and i from 0 do
                (if (char= c #\#)
                    (setf (gethash (list i j 0) active-cells) 0))))

    (loop for i from 0 below timesteps do
          (let ((new-grid (step-3d-automaton active-cells)))
            ; (print-grid new-grid)
            (setf active-cells new-grid)))

    (hash-table-count active-cells)))

(defun day17b (file)
  (let ((input (get-file file))
        (active-cells (make-hash-table :test #'equal))
        (timesteps 6))
    (loop for row in input and j from 0 do
          (loop for c across row and i from 0 do
                (if (char= c #\#)
                    (setf (gethash (list i j 0 0) active-cells) 0))))
    
    (loop for i from 0 below timesteps do
         (setf active-cells (step-4d-automaton active-cells)))

    (hash-table-count active-cells)))

(defun day17 ()
  (format t "17: ~A, ~A" (day17a "input17.txt") (day17b "input17.txt")))

;; ---------------------------- ;;
;;   DAY 18 - OPERATION ORDER   ;;
;; ---------------------------- ;;

(defun make-queue ()
  (let ((q (list nil)))
    (cons q q)))

(defun queue-contents (q)
  (cdar q))

(defun empty-queue-p (q)
  (null (cdar q)))

(defun queue-head (q)
  (cadar q))

(defun dequeue (q)
  (car (setf (car q) (cdar q))))

(defun enqueue (q item)
  (setf (cdr q) (setf (cddr q) (list item))))

(defun shunting-yard (line)
  (with-input-from-string (s line)
    (let (stack
          (queue (make-queue))
          c)

      (loop while (setf c (read-char s nil nil)) do
            (cond ((digit-char-p c) (enqueue queue (digit-char-p c)))
                  ((char= c #\*) (push #\* stack))
                  ((char= c #\+) (push #\+ stack))
                  ((char= c #\() (push #\( stack))
                  ((char= c #\)) (loop while (not (char= (first stack) #\()) do
                                       (enqueue queue (pop stack)))
                                 (pop stack)))

            )
      
      (loop while stack do (enqueue queue (pop stack)))

      (queue-contents queue))))

(defun eval-rpn (rpn)
  (let (stack c)
    (loop while rpn do
          (setf c (pop rpn))
          (cond ((numberp c) (push c stack))
                ((equal c #\+) (push (+ (pop stack) (pop stack)) stack))
                ((equal c #\*) (push (* (pop stack) (pop stack)) stack))))
    (first stack)))

(defun line-to-rpn (line)
  (eval-rpn (shunting-yard line)))

(defun eval-line (line)
  (with-input-from-string (s line)
    (let (operands
          operators
          ch)
      (loop while (setf ch (read-char s nil nil)) do
            (cond 
              ((digit-char-p ch) (push (digit-char-p ch) operands))

              ((or (char= ch #\*) (char= ch #\+)) 
               (if (equal (type-of (first operators)) (type-of #'+))
                   (progn
                     (push (funcall (pop operators) (pop operands) (pop operands))
                           operands)
                     (push (if (char= ch #\*) #'* #'+) operators))
                   (push (if (char= ch #\*) #'* #'+) operators)))

              ;((char= c #\+) (push #\+ stack))
              ((char= ch #\() (push #\( operators))
              ((char= ch #\)) (loop while (not (equal (first operators) #\()) do
                                    (push (funcall (pop operators)
                                                   (pop operands) (pop operands))
                                          operands))

                              ;; remove the '('
                              (pop operators))))

      (loop while operators do
            (push (funcall (pop operators) (pop operands) (pop operands)) operands))

      (first operands))))

(defun eval-line-2 (line)
  (with-input-from-string (s line)
    (let (operands
          operators
          ch)
      (loop while (setf ch (read-char s nil nil)) do
            (cond 
              ((digit-char-p ch) (push (digit-char-p ch) operands))

              ((or (char= ch #\*) (char= ch #\+)) 
               (if (equal (first operators) #'+)
                   (progn
                     (push (funcall (pop operators) (pop operands) (pop operands))
                           operands)
                     (push (if (char= ch #\*) #'* #'+) operators))
                   (push (if (char= ch #\*) #'* #'+) operators)))

              ;((char= c #\+) (push #\+ stack))
              ((char= ch #\() (push #\( operators))
              ((char= ch #\)) (loop while (not (equal (first operators) #\()) do
                                    (push (funcall (pop operators)
                                                   (pop operands) (pop operands))
                                          operands))

                              ;; remove the '('
                              (pop operators))))

      (loop while operators do
            (push (funcall (pop operators) (pop operands) (pop operands)) operands))

      (first operands))))

(defun day18a (file)
  (let ((input (get-file file)))
    (setf input (mapcar #'(lambda (l) (remove #\Space l)) input))
    (apply #'+ (mapcar #'eval-line input))))

(defun day18b (file)
  (let ((input (get-file file)))
    (setf input (mapcar #'(lambda (l) (remove #\Space l)) input))
    (apply #'+ (mapcar #'eval-line-2 input))))

(defun day18 ()
  (format t "18: ~D, ~D" (day18a "input18.txt") (day18b "input18.txt")))

(defun main ()
  (let ((start (get-internal-run-time))
        end)
    (day1) (day2) (day3) (day4) (day5) (day6) 
    (day7) (day8) (day9) (day10) (day11) (day12)
    (day13) (day14) (day15) (day16) (day17) (day18)

    ;; rest of the days...

    (setf end (get-internal-run-time))
    (format t "Total time: ~F~%" (/ (- end start ) 1000))))

; (sb-ext:save-lisp-and-die "aoc"
;                           :executable t
;                           :toplevel 'main)
