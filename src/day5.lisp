(defpackage day5 (:use :cl :iterate :utils))
(in-package :day5)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day5.txt"))
    (iter (for line in-lines stream) (collect line))))

(defun search-range (l lower upper lower-half-ch upper-half-ch)
  (let ((mid (truncate (/ (+ lower upper) 2))))
    (cond
      ((char= (car l) lower-half-ch)
       (if (= mid lower)
           lower
           (search-range (cdr l) lower mid lower-half-ch upper-half-ch)))
      ((char= (car l) upper-half-ch)
       (if (= mid lower)
           upper
           (search-range (cdr l) (+ mid 1) upper lower-half-ch upper-half-ch))))))

(defun decode (code)
  (let* ((l (iter (for ch in-string code) (collect ch)))
         (row-code (subseq l 0 7))
         (col-code (subseq l 7 (length l)))
         (row (search-range row-code 0 127 #\F #\B))
         (col (search-range col-code 0 7 #\L #\R)))
    (values row col)))

(defun seat-id (row col)
  (+ (* row 8) col))

(defparameter *part1*
  (iter (for code in *input*)
    (for (values row col) = (decode code))
    (maximize (seat-id row col))))

(defparameter *part2*
  (let ((id-map (iter (for code in *input*)
                  (for (values row col) = (decode code))
                  (collect (seat-id row col) => t))))
    (iter outer
      (for row from 0 to 127)
      (iter (for col from 0 to 7)
        (for id = (seat-id row col))
        (when (and (not (gethash id id-map))
                   (gethash (+ id 1) id-map)
                   (gethash (- id 1) id-map))
          (return-from outer id))))))
