(defpackage day6
  (:use :cl :iterate :utils)
  (:import-from :fset))
(in-package :day6)

(defvar *input*
  (with-open-file (stream #P"../resources/day6.txt")
    (split-list (iter (for line in-lines stream) (collect line)))))

(defun total-questions (answers reducer)
  (reduce reducer
          (mapcar (lambda (answer) (fset:convert 'fset:set answer)) answers)))

(defun sum-counts (input reducer)
  (iter (for answers in input)
        (summing (fset:size (total-questions answers reducer)))))

(defparameter *part1* (sum-counts *input* #'fset:union))
(defparameter *part2* (sum-counts *input* #'fset:intersection))
