(defpackage day8 (:use :cl :iterate :utils))
(in-package :day8)

(defparameter *input*
  (with-open-file (stream (relative-path #P"resources/day8.txt"))
    (let ((instrs (iter (for line in-lines stream)
                    (for (instr amt) = (uiop:split-string line :separator " "))
                    (collect (list instr (parse-integer amt))))))
      (make-array (length instrs) :initial-contents instrs))))

(defun interpret-instrs-turn (i acc instrs)
  (destructuring-bind (instr amt) (aref instrs i)
    (cond ((string= instr "acc") (values (+ i 1) (+ acc amt)))
          ((string= instr "jmp") (values (+ i amt) acc))
          ((string= instr "nop") (values (+ i 1) acc)))))

(defun check-terminate (input)
  (iter (with i = 0)
    (with acc = 0)
    (when (gethash i already-been-map)
      (return (values acc already-been-map nil)))
    (when (>= i (length input))
      (return (values acc already-been-map t)))
    (collecting i => t into already-been-map)
    (multiple-value-setq (i acc) (interpret-instrs-turn i acc input))))

(defun potential-changes (input)
  (multiple-value-bind (acc already-been-map) (check-terminate input)
    (declare (ignore acc))
    (iter (for (i _) in-hashtable already-been-map)
      (for (instr amt) = (aref input i))
      (when (or (string= instr "jmp")
                (and (string= instr "nop") (not (= amt 0))))
        (collect i)))))

(defun toggle-instr (instrs i)
  (destructuring-bind (instr amt) (aref instrs i)
    (cond ((string= instr "jmp") (setf (aref instrs i) (list "nop" amt)))
          ((string= instr "nop") (setf (aref instrs i) (list "jmp" amt))))))

(defun find-change (input)
  (iter (for i in (potential-changes input))
    (toggle-instr input i)
    (for (values acc _ works) = (check-terminate input))
    (toggle-instr input i)
    (when works (return acc))))

(defparameter *part1* (check-terminate *input*))
(defparameter *part2* (find-change *input*))
