(defpackage day9 (:use :cl :iterate :utils))
(in-package :day9)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day9.txt"))
    (let ((l (iter (for line in-lines stream)
               (collect (parse-integer line)))))
      (make-array (length l) :initial-contents l))))

(defun make-num-map (arr begin end)
  (iter (for num in-vector arr from begin to end) (collect num => t)))

(defun twosum (target arr begin end)
  (iter (with num-map = (make-num-map arr begin end))
    (for num in-vector arr from begin to end)
    (when (and (gethash (- target num) num-map)
               (not (= num (- target num))))
      (return t))))

(defun manysum (target arr begin end)
  (= (iter (for num in-vector arr from begin to end)
       (summing num))
     target))

(defun contiguous-range (target input)
  (iter outer
    (for begin from 0 below (length input))
    (iter (for end from (+ begin 1) below (length input))
      (when (manysum target input begin end)
        (return-from outer (values begin end))))))

(defun encryption-weakness (arr begin end)
  (iter (for num in-vector arr from begin to end)
    (minimizing num into min)
    (maximizing num into max)
    (finally (return (+ min max)))))

(defparameter *part1*
  (iter (for begin first 0 then (+ begin 1))
    (for end first 24 then (+ end 1))
    (for num in-vector *input* from 25)
    (when (not (twosum num *input* begin end)) (return num))))

(defparameter *part2*
  (multiple-value-bind (begin end) (contiguous-range *part1* *input*)
    (encryption-weakness *input* begin end)))
