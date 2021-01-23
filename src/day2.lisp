(defpackage day2 (:use :cl :iterate :utils))
(in-package :day2)

(defun parse-range (range)
  (mapcar #'parse-integer (uiop:split-string range :separator "-")))

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day2.txt"))
    (iter (for line in-lines stream)
          (destructuring-bind (letter-repeat-range letter password)
              (uiop:split-string line :max 3 :separator " ")
            (collect (list (parse-range letter-repeat-range)
                           (char (string-right-trim ":" letter) 0)
                           password))))))

(defun count-letters (letter str)
  (iter (with letter-count = (make-hash-table))
        (for ch in-string str)
        (multiple-value-bind (count present) (gethash ch letter-count)
          (if present
              (setf (gethash ch letter-count) (+ count 1))
              (setf (gethash ch letter-count) 1)))
        (finally (return (gethash letter letter-count 0)))))

(defun valid-policy (policy)
  (destructuring-bind ((min-times max-times) letter password) policy
    (let ((letter-count (count-letters letter password)))
      (and (>= letter-count min-times) (<= letter-count max-times)))))

(defun valid-policy2 (policy)
  (destructuring-bind ((pos1 pos2) letter password) policy
    (let ((i1 (- pos1 1)) (i2 (- pos2 1)))
      (xor
       (char= (char password i1) letter)
       (char= (char password i2) letter)))))

(defparameter *part1*
  (iter (for policy in *input*)
        (counting (valid-policy policy))))

(defparameter *part2*
  (iter (for policy in *input*)
        (counting (valid-policy2 policy))))
