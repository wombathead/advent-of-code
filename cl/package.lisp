(defpackage #:aoc
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:str
                #:words
                #:split)
  
  (:export #:read-from-file
           #:empty-string-p
           #:char-difference

           #:aoc-2021-01a #:aoc-2021-01b
           #:aoc-2021-02a #:aoc-2021-02b
           #:aoc-2021-03a #:aoc-2021-03b
           #:aoc-2021-04a #:aoc-2021-04b
           #:aoc-2021-05a #:aoc-2021-05b
           #:aoc-2021-06a #:aoc-2021-06b
           #:aoc-2021-07a #:aoc-2021-07b
           
           #:aoc-2022-01a #:aoc-2022-01b  
           #:aoc-2022-02a #:aoc-2022-02b  
           #:aoc-2022-03a #:aoc-2022-03b  
           #:aoc-2022-04a #:aoc-2022-04b  
           #:aoc-2022-05a #:aoc-2022-05b  
           #:aoc-2022-06a #:aoc-2022-06b  
           #:aoc-2022-08a #:aoc-2022-08b  
           #:aoc-2022-10a #:aoc-2022-10b  
           #:aoc-2022-12a #:aoc-2022-12b))
