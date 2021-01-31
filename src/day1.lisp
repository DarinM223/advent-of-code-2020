(defpackage day1 (:use :cl :iterate :utils))
(in-package :day1)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day1.txt"))
    (iter (for line in-lines stream)
      (collect (parse-integer line)))))

(defun make-num-map (nums result-sum)
  (iter (for num in nums) (collect num => (- result-sum num))))

(defun two-numbers-sum (nums result-sum)
  (let ((num-map (make-num-map nums result-sum)))
    (iter (for (k v) in-hashtable num-map)
      (when (gethash v num-map)
        (collect (list k v))))))

; Hashmap's key needs to be only one number to check the third value.
; Keep the hashmap the same as in part 1 but loop through all
; valid pairs of numbers and only check the third number.
(defun three-numbers-sum (nums result-sum)
  (let ((num-map (make-num-map nums result-sum)))
    (iter outer
      (for num1 in nums)
      (iter (for num2 in nums)
        (for num3 = (- result-sum num1 num2))
        (in outer
            (when (gethash num3 num-map)
              (collect (list num1 num2 num3))))))))

(defparameter *part1*
  (destructuring-bind (a b)
      (first (two-numbers-sum *input* 2020))
    (* a b)))

(defparameter *part2*
  (destructuring-bind (a b c)
      (first (three-numbers-sum *input* 2020))
    (* a b c)))
