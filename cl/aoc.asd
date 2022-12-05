(defsystem "aoc"
  :depends-on (:alexandria :str :cl-ppcre)
  :components ((:file "package")
               (:file "util")

               (:file "2021/01")
               (:file "2021/02")
               (:file "2021/03")
               (:file "2021/04")
               (:file "2021/05")
               (:file "2021/06")
               (:file "2021/07")
               (:file "2021/08")

               (:file "2022/01")
               (:file "2022/02")  
               (:file "2022/03")  
               (:file "2022/04")  
               (:file "2022/05")))
