(defpackage day11 (:use :cl :arrows :iterate :utils))
(in-package :day11)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day11.txt"))
    (let ((l (iter (for line in-lines stream) (collect line))))
      (make-array (list (length l) (length (first l))) :initial-contents l))))

(defun grid-eq (g1 g2)
  (destructuring-bind (h1 w1) (array-dimensions g1)
    (destructuring-bind (h2 w2) (array-dimensions g2)
      (and (= h1 h2) (= w1 w2)
           (iter outer
             (for i from 0 below w1)
             (iter (for j from 0 below h1)
               (in outer (always (char= (aref g1 j i) (aref g2 j i))))))))))

(defun out-of-bounds (p grid)
  (destructuring-bind (height width) (array-dimensions grid)
    (destructuring-bind (y x) p
      (or (< y 0) (>= y height) (< x 0) (>= x width)))))

(defun count-occupied-adj (grid y x)
  (->> (list (list (+ y 1) x)
             (list (- y 1) x)
             (list y (+ x 1))
             (list y (- x 1))
             (list (+ y 1) (+ x 1))
             (list (+ y 1) (- x 1))
             (list (- y 1) (+ x 1))
             (list (- y 1) (- x 1)))
       (remove-if (lambda (p) (out-of-bounds p grid)))
       (mapcar (lambda (p) (apply #'aref (cons grid p))))
       (count-if (lambda (seat) (char= seat #\#)))))

(defun seat (grid y x)
  (let ((v (aref grid y x)))
    (cond ((and (char= v #\L) (= (count-occupied-adj grid y x) 0)) #\#)
          ((and (char= v #\#) (>= (count-occupied-adj grid y x) 4)) #\L)
          (t v))))

(defun count-occupied-look (grid y x)
  (flet ((first-seat-in-direction (y x direction-fn)
           (iter
             (with j = y)
             (with i = x)
             (multiple-value-setq (j i) (funcall direction-fn j i))
             (cond ((out-of-bounds (list j i) grid) (return #\.))
                   ((not (char= (aref grid j i) #\.)) (return (aref grid j i)))))))
    (let ((directions (list (lambda (y x) (values (+ y 1) x))
                            (lambda (y x) (values (- y 1) x))
                            (lambda (y x) (values y (+ x 1)))
                            (lambda (y x) (values y (- x 1)))
                            (lambda (y x) (values (+ y 1) (+ x 1)))
                            (lambda (y x) (values (+ y 1) (- x 1)))
                            (lambda (y x) (values (- y 1) (+ x 1)))
                            (lambda (y x) (values (- y 1) (- x 1))))))
      (iter (for direction in directions)
        (counting (char= (first-seat-in-direction y x direction) #\#))))))

(defun seat2 (grid y x)
  (let ((v (aref grid y x)))
    (cond ((and (char= v #\L) (= (count-occupied-look grid y x) 0)) #\#)
          ((and (char= v #\#) (>= (count-occupied-look grid y x) 5)) #\L)
          (t v))))

(defun seat-round (grid seat-fn)
  (destructuring-bind (height width) (array-dimensions grid)
    (iter outer
      (with new-grid = (make-array (list height width) :initial-element #\.))
      (for x from 0 below width)
      (iter (for y from 0 below height)
        (setf (aref new-grid y x) (funcall seat-fn grid y x)))
      (finally (return-from outer new-grid)))))

(defun apply-rounds (grid seat-fn)
  (iter
    (for g first grid then new-grid)
    (for new-grid = (seat-round g seat-fn))
    (while (not (grid-eq g new-grid)))
    (finally (return g))))

(defun count-occupied (grid)
  (destructuring-bind (height width) (array-dimensions grid)
    (iter outer
      (for i from 0 below width)
      (iter (for j from 0 below height)
        (in outer (counting (char= (aref grid j i) #\#)))))))

(defparameter *part1* (count-occupied (apply-rounds *input* #'seat)))
(defparameter *part2* (count-occupied (apply-rounds *input* #'seat2)))
