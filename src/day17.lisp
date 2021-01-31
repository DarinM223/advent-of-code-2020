(defpackage day17 (:use :cl :iterate :utils))
(in-package :day17)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day17.txt"))
    (coerce (iter (for line in-lines stream) (collect line)) 'vector)))

(defun neighbors (p)
  (labels ((rec (p) (and p (iter outer
                             (with num = (car p))
                             (with rest = (rec (cdr p)))
                             (for i in (list (- num 1) num (+ num 1)))
                             (if rest
                                 (iter (for np in rest)
                                   (in outer (collect (cons i np))))
                                 (collect (list i)))))))
    (remove-if (lambda (np) (equal np p)) (rec p))))

(defun points-in-ranges (ranges)
  (and ranges
       (iter outer
         (with (range-min range-max) = (car ranges))
         (with rest = (points-in-ranges (cdr ranges)))
         (for dim from range-min to range-max)
         (if rest
             (iter (for np in rest) (in outer (collect (cons dim np))))
             (collect (list dim))))))

(defun run-cycle (active-points ranges)
  (iter (for p in (points-in-ranges ranges))
    (let ((active-neighbors (iter (for n in (neighbors p))
                              (counting (gethash n active-points)))))
      (if (gethash p active-points)
          (when (or (= active-neighbors 2) (= active-neighbors 3))
            (collect p => t test #'equal))
          (when (= active-neighbors 3)
            (collect p => t test #'equal))))))

(defun run-cycles (input dimensions turns)
  (let ((active-points-init
          (iter outer
            (for line in-vector input with-index y)
            (for padding = (iter (repeat (- dimensions 2)) (collect 0)))
            (iter (for ch in-string line with-index x)
              (for index = (append (list x y) padding))
              (when (char= ch #\#)
                (in outer (collect index => t test #'equal)))))))
    (iter (with active-points = active-points-init)
      (with ranges = (append (list '(0 7) '(0 7))
                             (iter (repeat (- dimensions 2))
                               (collect (list -1 1)))))
      (repeat turns)
      (setf active-points (run-cycle active-points ranges))
      (setf ranges (iter (for (min max) in ranges)
                     (collect (list (- min 1) (+ max 1)))))
      (finally (return active-points)))))

(defparameter *part1* (hash-table-count (run-cycles *input* 3 6)))
(defparameter *part2* (hash-table-count (run-cycles *input* 4 6)))
