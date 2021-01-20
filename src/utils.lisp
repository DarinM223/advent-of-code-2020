(defpackage utils
  (:use :cl :iterate)
  (:export :xor))
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
