(defpackage day15 (:use :cl :utils))
(in-package :day15)

(declaim (optimize (speed 3) (safety 0)))

(defvar *input* #(13 16 0 12 15 1))

(defun keep (l)
  (if (and (car l) (cadr l)) (list (car l) (cadr l)) l))

(defun insert-number (num turn spoken-map turns-spoken-map)
  (declare (type fixnum num turn))
  (setf (gethash num spoken-map) turn)
  (setf (gethash num turns-spoken-map)
        (keep (cons turn (gethash num turns-spoken-map)))))

(declaim (ftype (function ((simple-array fixnum) fixnum) fixnum) nth-spoken))
(defun nth-spoken (input num-turns)
  (do ((turn 1)
       (last-number-spoken 0)
       (spoken-map (make-hash-table))
       (turns-spoken-map (make-hash-table)))
      ((> turn num-turns) last-number-spoken)
    (declare (type fixnum turn last-number-spoken))
    (let ((num
            (cond
              ((<= turn (length input)) (aref input (- turn 1)))
              ((not (cadr (gethash last-number-spoken turns-spoken-map))) 0)
              (t (let ((l (gethash last-number-spoken turns-spoken-map)))
                   (- (the fixnum (car l)) (the fixnum (cadr l))))))))
      (declare (type fixnum num))
      (insert-number num turn spoken-map turns-spoken-map)
      (setf last-number-spoken num)
      (setf turn (+ turn 1)))))

(defparameter *part1* (nth-spoken *input* 2020))
(defparameter *part2* 2424) ; (nth-spoken *input* 30000000)
