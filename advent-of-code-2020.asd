(defsystem "advent-of-code-2020"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("iterate")
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "day1")
                 (:file "day2")
                 (:file "day3")
                 (:file "day4"))))
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
