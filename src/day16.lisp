(defpackage day16 (:use :cl :iterate :utils))
(in-package :day16)

(defun parse-rule (line)
  (flet ((parse-range (line)
           (mapcar #'parse-integer (uiop:split-string line :separator "-"))))
    (let ((l (uiop:split-string (cadr (uiop:split-string line :separator ":"))
                                :separator "or")))
      (list (parse-range (car l)) (parse-range (caddr l))))))

(defun parse-ticket (line)
  (coerce (mapcar #'parse-integer (uiop:split-string line :separator ","))
          'vector))

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day16.txt"))
    (destructuring-bind (rules ticket nearby)
        (split-list (iter (for line in-lines stream) (collect line)))
      (list (mapcar #'parse-rule rules)
            (parse-ticket (cadr ticket))
            (mapcar #'parse-ticket (cdr nearby))))))
(defparameter *rules* (car *input*))
(defparameter *your-ticket* (cadr *input*))
(defparameter *nearby-tickets* (caddr *input*))

(defun valid-numberp (num rules)
  (iter (for ((low1 hi1) (low2 hi2)) in rules)
    (thereis (or (and (>= num low1) (<= num hi1))
                 (and (>= num low2) (<= num hi2))))))

(defun sum-invalid (ticket rules)
  (iter (for num in-vector ticket)
    (when (not (valid-numberp num rules)) (summing num))))

(defun validp (ticket rules)
  (iter (for num in-vector ticket) (always (valid-numberp num rules))))

(defparameter *valid-tickets*
  (remove-if-not (lambda (ticket) (validp ticket *rules*)) *nearby-tickets*))

(defun match-rules (rule-index build numbers guesses)
  (cond ((= (hash-table-count numbers) 0) build)
        (t (dolist (guess (aref guesses rule-index))
             (when (gethash guess numbers)
               (remhash guess numbers)
               (setf (gethash rule-index build) guess)
               (let ((rest (match-rules (+ rule-index 1) build numbers guesses)))
                 (setf (gethash guess numbers) t)
                 (when rest (return rest))
                 (remhash rule-index build)))))))

(defun make-rules-to-ticket-index-map (rules tickets)
  (flet ((ticket-choices (rule)
           (iter (for i from 0 below (length (car tickets)))
             (when (iter (for ticket in tickets)
                     (always (valid-numberp (aref ticket i) (list rule))))
               (collect i)))))
    (let ((possible-choices (coerce (mapcar #'ticket-choices rules) 'vector))
          (ticket-numbers (iter (with map = (make-hash-table))
                            (for i from 0 below (length (car tickets)))
                            (setf (gethash i map) t)
                            (finally (return map)))))
      (match-rules 0 (make-hash-table) ticket-numbers possible-choices))))

(defparameter *part1*
  (iter (for ticket in *nearby-tickets*) (summing (sum-invalid ticket *rules*))))
(defparameter *part2*
  (iter (with map = (make-rules-to-ticket-index-map *rules* *valid-tickets*))
    (for rule from 0 to 5)
    (for ticket-index = (gethash rule map))
    (multiplying (aref *your-ticket* ticket-index))))
