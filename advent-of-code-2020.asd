(defsystem "advent-of-code-2020"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("alexandria"
               "arrows"
               "cl-ppcre"
               "infix-math"
               "iterate"
               "fset"
               "trivia")
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "day1")
                 (:file "day2")
                 (:file "day3")
                 (:file "day4")
                 (:file "day5")
                 (:file "day6")
                 (:file "day7")
                 (:file "day8")
                 (:file "day9")
                 (:file "day10")
                 (:file "day11")
                 (:file "day12")
                 (:file "day13")
                 (:file "day14")
                 (:file "day15")
                 (:file "day16")
                 (:file "day17")
                 (:file "day18")
                 (:file "day19")
                 (:file "day20"))))
  :description ""
  :in-order-to ((test-op (test-op "advent-of-code-2020/tests"))))

(defsystem "advent-of-code-2020/tests"
  :author ""
  :license ""
  :depends-on ("advent-of-code-2020"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for advent-of-code-2020"
  :perform (test-op (op c) (symbol-call :rove :run c)))
