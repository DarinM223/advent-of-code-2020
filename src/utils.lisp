(defpackage utils
  (:use :cl :iterate)
  (:export :xor :split-list :relative-path))
(in-package :utils)

(defmacro-clause (FOR var in-lines stream)
  "Over lines in a stream"
  `(progn
     (for ,var = (read-line ,stream nil))
     (while ,var)))

; Examples:
;
; (iter (for i from 0 to 3)
;   (for j from 0 to 3)
;   (collect i => j))
;
; (iter (for i in '("a" "b" "c" "d"))
;   (for j from 0 to 3)
;   (collecting i => j into hash test #'equal)
;   (finally (return (gethash "d" hash))))
;
; (iter (for i from 0 to 4) (for j from 0 to 4)
;   (if (= (mod i 2) 0)
;       (collect i => (* j 2))
;       (collect i => j))))
(defmacro-clause (COLLECT key => value &optional INTO var TEST test)
  "Collects key-value tuple into hash table"
  (let* ((table-spec (or var iterate::*result-var*))
         (table (iterate::extract-var table-spec)))
    (if (car (member table iterate::*bindings* :test #'eq :key #'car))
        `(setf (gethash ,key ,table) ,value)
        `(progn
           (with ,table = (if ,test
                              (make-hash-table :test ,test)
                              (make-hash-table)))
           (setf (gethash ,key ,table) ,value)))))

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
