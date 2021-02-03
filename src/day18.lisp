(defpackage day18 (:use :cl :iterate :utils))
(in-package :day18)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day18.txt"))
    (iter (for line in-lines stream) (collect line))))

(defun evaluate-line (line)
  (eval (read-from-string (format nil "~a~a~a" "(infix-math:$ " line ")"))))

(defun <*> (x y) "Custom multiplication for part 1" (* x y))
(infix-math:declare-binary-operator <*> :from +)
(defun <+> (x y) "Custom addition for part 2" (+ x y))
(infix-math:declare-binary-operator <+> :from *)

(defun replace-string (s plus-replace times-replace)
  (cl-ppcre:regex-replace-all "[*]"
                              (cl-ppcre:regex-replace-all "[+]" s plus-replace)
                              times-replace))

(defparameter *part1*
  (iter (for line in *input*)
    (summing (evaluate-line (replace-string line "+" "<*>")))))
(defparameter *part2*
  (iter (for line in *input*)
    (summing (evaluate-line (replace-string line "<+>" "<*>")))))
