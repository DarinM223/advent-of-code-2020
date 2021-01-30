(defpackage day17 (:use :cl :iterate :utils))
(in-package :day17)

(defparameter *input*
  (with-open-file (stream (relative-path #P"resources/day17.txt"))
    (coerce (iter (for line in-lines stream) (collect line)) 'vector)))
