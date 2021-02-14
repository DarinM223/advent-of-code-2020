(defpackage day6
  (:use :cl :arrows :iterate :utils)
  (:import-from :alexandria :curry))
(in-package :day6)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day6.txt"))
    (split-list (iter (for line in-lines stream) (collect line)))))

(defun total-questions (answers reducer)
  (->> answers
       (mapcar (curry #'fset:convert 'fset:set))
       (reduce reducer)))

(defun sum-counts (input reducer)
  (iter (for answers in input)
    (summing (fset:size (total-questions answers reducer)))))

(defparameter *part1* (sum-counts *input* #'fset:union))
(defparameter *part2* (sum-counts *input* #'fset:intersection))
