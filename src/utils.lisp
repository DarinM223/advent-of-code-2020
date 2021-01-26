(defpackage utils
  (:use :cl :iterate)
  (:export :xor :split-list :relative-path))
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

(defun split-list (l &key (separator ""))
  "Splits list using strings as separators"
  (iter (with start = 0)
    (for str in-sequence l with-index i)
    (when (string= str separator)
      (progn
        (collect (subseq l start i))
        (setf start (+ i 1))))))

(defun relative-path (path)
  "Given a path relative to the project's path, return the full path"
  (asdf:system-relative-pathname (asdf:find-system :advent-of-code-2020) path))
