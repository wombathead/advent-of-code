(defpackage #:aoc
  (:use #:cl #:iterate)

  (:import-from #:alexandria
                #:hash-table-keys)

  (:export #:read-from-file

           #:aoc-2024-01a #:aoc-2024-01b
           #:aoc-2024-02a #:aoc-2024-02b
           #:aoc-2024-03a #:aoc-2024-03b
           #:aoc-2024-04a #:aoc-2024-04b
           #:aoc-2024-05a #:aoc-2024-05b

           #:aoc-2023-01a #:aoc-2023-01b
           #:aoc-2023-02a #:aoc-2023-02b
           #:aoc-2023-03a #:aoc-2023-03b
           #:aoc-2023-04a #:aoc-2023-04b
           #:aoc-2023-05a #:aoc-2023-05b
           #:aoc-2023-06a #:aoc-2023-06b

           #:aoc-2022-01a #:aoc-2022-01b
           #:aoc-2022-02a #:aoc-2022-02b  
           #:aoc-2022-03a #:aoc-2022-03b  
           #:aoc-2022-04a #:aoc-2022-04b  
           #:aoc-2022-05a #:aoc-2022-05b  
           #:aoc-2022-06a #:aoc-2022-06b  
           #:aoc-2022-08a #:aoc-2022-08b  
           #:aoc-2022-10a #:aoc-2022-10b  
           #:aoc-2022-12a #:aoc-2022-12b  
           #:aoc-2022-13a #:aoc-2022-13b  
           #:aoc-2022-14a #:aoc-2022-14b  
           #:aoc-2022-18a #:aoc-2022-18b

           #:aoc-2021-01a #:aoc-2021-01b
           #:aoc-2021-02a #:aoc-2021-02b
           #:aoc-2021-03a #:aoc-2021-03b
           #:aoc-2021-04a #:aoc-2021-04b
           #:aoc-2021-05a #:aoc-2021-05b
           #:aoc-2021-06a #:aoc-2021-06b
           #:aoc-2021-07a #:aoc-2021-07b))
