(defpackage utils
  (:use :cl :iterate)
  (:export :xor :split-list :fix-debug))
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

(defun fix-debug (v)
  "Fixes an annoyance with SBCL's debugger where certain values
in let bindings won't show up in the debugger's list of
local values for that frame.

If you run this:

```
(defun foo ()
  (declare (optimize (debug 3)))
  (let ((a blah))
    (break)
    (print a)))

(foo)
```

And `a` doesn't show up in the locals list for the `(FOO)` frame,
then try wrapping `blah` with fix-debug:

```
(defun foo ()
  (declare (optimize (debug 3)))
  (let ((a (fix-debug blah)))
    (break)
    (print a)))

(foo)
```
  "
  (first (list v)))
