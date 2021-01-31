(defpackage day3 (:use :cl :iterate :utils))
(in-package :day3)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day3.txt"))
    (let ((lines (iter (for line in-lines stream)
                   (collect line))))
      (make-array (length lines) :initial-contents lines))))

(defun width (grid)
  (length (aref grid 0)))

(defun wrap-x (x grid)
  (if (>= x (width grid))
      (wrap-x (- x (width grid)) grid)
      x))

(defun grid-char-at (grid y x)
  (char (aref grid y) (wrap-x x grid)))

(defun count-trees (grid y-fn x-fn)
  (iter (for y first 0 then (setf y (funcall y-fn y)))
    (for x first 0 then (setf x (funcall x-fn x)))
    (while (< y (length grid)))
    (when (> y 0)
      (counting (char= (grid-char-at grid y x) #\#)))))

(defparameter *part1*
  (count-trees *input* (lambda (y) (+ y 1)) (lambda (x) (+ x 3))))

(defparameter *part2*
  (* (count-trees *input* (lambda (y) (+ y 1)) (lambda (x) (+ x 1)))
     (count-trees *input* (lambda (y) (+ y 1)) (lambda (x) (+ x 3)))
     (count-trees *input* (lambda (y) (+ y 1)) (lambda (x) (+ x 5)))
     (count-trees *input* (lambda (y) (+ y 1)) (lambda (x) (+ x 7)))
     (count-trees *input* (lambda (y) (+ y 2)) (lambda (x) (+ x 1)))))
