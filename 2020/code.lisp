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

(defun xor (a b)
  (and
    (or a b)
    (not (and a b))))

(defun within-range (num lower upper)
  " return T if NUM is between LOWER and UPPER "
  (and (>= num lower) (<= num upper)))

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

  (format t "~A~%" (day1a "input1.txt"))
  (format t "~A~%" (day1b "input1.txt")))

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
    (setf height (first dimensions))
    (setf width (second dimensions))

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
                       (progn (setf (aref new-grid j i) #\#)
                              (setf changed? T))))
                  (#\#
                   (if (>= (adjacent-occupied-seats grid j i) 4)
                       (progn (setf (aref new-grid j i) #\L)
                              (setf changed? T)))))))
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
                       (progn (setf (aref new-grid j i) #\#)
                              (setf changed? T))))
                  (#\#
                   (if (>= (visible-occupied-seats grid j i) 5)
                       (progn (setf (aref new-grid j i) #\L)
                              (setf changed? T)))))))
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
          (setf s (vec2:add s (vec2:mult dst diff)))
          (setf w (vec2:add s diff))))

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
          (setf (values ship waypoint)
                (move-waypoint ship waypoint instr)))
    (vec2:l1 ship origin)))

(defun day12 ()
  (format t "~D~%" (day12a "input12.txt"))
  (format t "~D~%" (day12b "input12.txt")))

(defun main ()
  (let ((start (get-internal-run-time))
        end)
    (day1) (day2) (day3) (day4) (day5) (day6) 
    (day7) (day8) (day9) (day10) (day11)

    ;; rest of the days...

    (setf end (get-internal-run-time))
    (format t "Total time: ~F~%" (/ (- end start ) 1000))))

;; ---------------------------- ;;
;;   DAY 13: SHUTTLE SEARCH     ;;
;; ---------------------------- ;;

(defun get-buses (file)
  (let ((input (get-file file))
        earliest-time
        buses)
    (setf earliest-time (parse-integer (first input)))
    (setf buses (str:split "," (second input)))
    (list earliest-time
          buses)))

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
    (setf minimum (first input))
    (setf buses (mapcar #'parse-integer
                        (remove-if #'(lambda (b)
                                           (equal b "x"))
                                       (second input))))
    (setf earliest-time
          (apply #'min (mapcar #'(lambda (b)
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
          (setf q (floor r- r))
          (setf (values r- r) (values r (- r- (* q r))))
          (setf (values s- s) (values s (- s- (* q s))))
          (setf (values u- u) (values u (- u- (* q u)))))

    (values s- u-)))

(defun crt (congruences)
  " given x = a1 (mod n1) = a2 (mod n2) ... supplied in the list ((a1 n1) ...) CONGRUENCES, compute x using the Chinese Remainder Theorem "
  (loop while (> (length congruences) 1) do
        (let ((d1 (pop congruences))
              (d2 (pop congruences))
              a1 m1 n1
              a2 m2 n2)

          (setf a1 (first d1))
          (setf n1 (second d1))
          (setf a2 (first d2))
          (setf n2 (second d2))

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

;(sb-ext:save-lisp-and-die "aoc"
;                          :executable t
;                          :toplevel 'main)
