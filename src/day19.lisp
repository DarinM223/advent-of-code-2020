(defpackage day19
  (:use :cl :arrows :iterate :utils)
  (:import-from :alexandria :curry))
(in-package :day19)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day19.txt"))
    (iter (with rules-finished = nil)
      (for line in-lines stream)
      (cond ((string= line "") (setf rules-finished t))
            (rules-finished (collect line into messages))
            (t (collect line into rules)))
      (finally (return (list rules messages))))))
(defparameter *rules* (car *input*))
(defparameter *messages* (cadr *input*))

(defun safe-parse-integer (s)
  (or (parse-integer s :junk-allowed t) s))

(defun parse-rule (line)
  (flet ((parse-left-hand (s)
           (iter (for branch in (uiop:split-string s :separator "|"))
             (collect (->> branch
                           (uiop:split-string)
                           (remove-if (curry #'string= ""))
                           (mapcar #'safe-parse-integer))))))
    (destructuring-bind (key value) (uiop:split-string line :separator ":")
      (values (parse-integer key) (parse-left-hand value)))))

(defparameter *rule-map*
  (iter (for rule in *rules*)
    (for (values key value) = (parse-rule rule))
    (collect key => value)))

(defparameter *rule-map2*
  (iter (for rule in *rules*)
    (for (values key value) = (parse-rule rule))
    (cond ((= key 8) (collect key => '((42) (42 8))))
          ((= key 11) (collect key => '((42 31) (42 11 31))))
          (t (collect key => value)))))

(defun character-rulep (rule)
  (and (car rule) (car (car rule)) (stringp (car (car rule)))))

(defun complete-match (rule-index message rule-table)
  (iter (for completed-pos in (match-rules rule-index (list 0) message rule-table))
    (thereis (= completed-pos (length message)))))

(defun match-rules (rule-index positions message rule-table)
  (let ((rule (gethash rule-index rule-table)))
    (if (character-rulep rule)
        (match-character-rule positions (string-trim "\" " (car (car rule))) message)
        (match-branches-rule positions rule message rule-table))))

(defun match-character-rule (positions rule message)
  (mapcar (lambda (pos) (+ pos 1))
          (remove-if (lambda (pos)
                       (or (>= pos (length message))
                           (not (char= (char rule 0) (char message pos)))))
                     positions)))

; Branches can return multiple valid positions.
(defun match-branches-rule (positions branches message rule-table)
  (iter outer
    (for branch in branches)
    (iter (for curr-positions first positions then new-positions)
      (for rule-index in branch)
      (for new-positions = (match-rules rule-index curr-positions message rule-table))
      (while (> (length new-positions) 0))
      (finally (when (> (length new-positions) 0)
                 (in outer (appending new-positions)))))))

(defparameter *part1*
  (iter (for message in *messages*)
    (counting (complete-match 0 message *rule-map*))))
(defparameter *part2*
  (iter (for message in *messages*)
    (counting (complete-match 0 message *rule-map2*))))
