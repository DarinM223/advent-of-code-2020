(defpackage day12
  (:use :cl :iterate :utils)
  (:import-from :trivia))
(in-package :day12)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day12.txt"))
    (iter (for line in-lines stream)
      (collect (list (char line 0)
                     (parse-integer (string-left-trim "NSEWLRF" line)))))))

(defun apply-move (direction amount north-amount west-amount)
  (trivia:match direction
    (#\N (values (+ north-amount amount) west-amount))
    (#\S (values (- north-amount amount) west-amount))
    (#\E (values north-amount (- west-amount amount)))
    (#\W (values north-amount (+ west-amount amount)))))

(defun turn (direction amount &key left)
  (if (= amount 0)
      direction
      (trivia:match direction
        (#\N (if left
                 (turn #\W (- amount 90) :left left)
                 (turn #\E (- amount 90) :left left)))
        (#\S (if left
                 (turn #\E (- amount 90) :left left)
                 (turn #\W (- amount 90) :left left)))
        (#\E (if left
                 (turn #\N (- amount 90) :left left)
                 (turn #\S (- amount 90) :left left)))
        (#\W (if left
                 (turn #\S (- amount 90) :left left)
                 (turn #\N (- amount 90) :left left))))))

(defun calc-distance (input)
  (iter (with direction = #\E)
    (with north = 0)
    (with west = 0)
    (for (action value) in input)
    (trivia:match action
      (#\F (multiple-value-setq (north west) (apply-move direction value north west)))
      (#\L (setf direction (turn direction value :left t)))
      (#\R (setf direction (turn direction value :left nil)))
      (_ (multiple-value-setq (north west) (apply-move action value north west))))
    (finally (return (+ (abs north) (abs west))))))

(defun apply-move-waypoint (amount north west north-diff west-diff)
  (if (= amount 0)
      (values north west)
      (apply-move-waypoint (- amount 1)
                           (+ north north-diff) (+ west west-diff)
                           north-diff west-diff)))

(defun turn-waypoint (amount north-diff west-diff &key left)
  (if (= amount 0)
      (values north-diff west-diff)
      (if left
          (turn-waypoint (- amount 90) (- west-diff) north-diff :left left)
          (turn-waypoint (- amount 90) west-diff (- north-diff) :left left))))

(defun calc-distance-waypoint (input)
  (iter (with north = 0)
    (with west = 0)
    (with north-diff = 1)
    (with west-diff = -10)
    (for (action value) in input)
    (trivia:match action
      (#\F (multiple-value-setq (north west)
             (apply-move-waypoint value north west north-diff west-diff)))
      (#\L (multiple-value-setq (north-diff west-diff)
             (turn-waypoint value north-diff west-diff :left t)))
      (#\R (multiple-value-setq (north-diff west-diff)
             (turn-waypoint value north-diff west-diff :left nil)))
      (_ (multiple-value-setq (north-diff west-diff)
           (apply-move action value north-diff west-diff))))
    (finally (return (+ (abs north) (abs west))))))

(defparameter *part1* (calc-distance *input*))
(defparameter *part2* (calc-distance-waypoint *input*))
