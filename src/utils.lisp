(defpackage utils
  (:use :cl :iterate)
  (:export :xor :split-list))
(in-package :utils)

(defmacro-clause (FOR var in-lines stream)
  "Over lines in a stream"
  `(progn
     (for ,var = (read-line ,stream nil))
     (while ,var)))

(defun xor (a b)
  "Logical exclusive or"
  (and (or a b)
       (not (and a b))))

(defun split-list (l)
  "Splits list using empty strings as separators"
  (iter (with start = 0)
        (for str in-sequence l with-index i)
        (when (string= str "")
          (progn
            (collect (subseq l start i))
            (setf start (+ i 1))))))
