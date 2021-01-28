(defpackage day13 (:use :cl :iterate :utils))
(in-package :day13)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day13.txt"))
    (let ((timestamp (parse-integer (read-line stream nil)))
          (schedule (uiop:split-string (read-line stream nil) :separator ",")))
      (list timestamp schedule))))

(defparameter *timestamp* (first *input*))
(defparameter *schedule*
  (let ((l (mapcar (lambda (e) (if (string= e "x") 0 (parse-integer e)))
                   (second *input*))))
    (make-array (length l) :initial-contents l)))

(defun bus-at-time (time schedule)
  (iter (for bus in-vector schedule)
    (when (and (not (= bus 0)) (= (mod time bus) 0)) (return bus))))

(defun earliest-bus (timestamp schedule)
  (iter (for time first timestamp then (+ time 1))
    (for bus = (bus-at-time time schedule))
    (when bus (return (values bus (- time timestamp))))))

; The solution to this part assumes that all of the bus id's are primes.
;
; Let's use the sample input #(7 13 0 0 59 0 31 19) as an example.
;
; At time = 77, 77 is divisible by 7 and 78 is divisible by 13.
; 81 is not divisible by 59, so 77 only works with 7 and 13.
;
; But if you now start at time = 77 and continously increment by LCM(7, 13), then
; every number from now on will also work with 7 and 13. This means you can
; focus on finding the first number in the new sequence that also works with 59.
;
; Now we find that at time = 350, 59 will also work. Now we can start at
; time = 350 and continuously increment it by LCM(7, 13, 59). Every number will
; automatically work with 7, 13, and 59 and we can check for a number that works
; with 31.
;
; We do this until there are no more bus ids left. The time
; when all buses are gone is the complete answer.
;
; Loop variables:
;
; `i` is the current index in the array of bus ids
; and is also used as the current offset. For example above, when `i` is 0,
; schedule[i] = 7 and for each time, you check if (time + 0) % 7 == 0.
; When `i` is 1, schedule[i] = 13 and you check if (time + 1) % 13 == 0.
; When `i` is 4, schedule[i] = 59 and you check if (time + 4) % 59 == 0.
; When schedule[i] = 0, i increments to skip that bus id.
;
; `incr` is the current amount to increment the time by.
; LCM(LCM(a, b), c) == LCM(a, b, c) so to add a new number to `incr`,
; you can LCM `incr` with the new number.
(defun earliest-timestamp (schedule)
  (do ((i 0)
       (incr 1)
       (time (aref schedule 0)))
      ((>= i (length schedule)) time)
    (let ((bus (aref schedule i)))
      (cond ((= bus 0) (setf i (+ i 1)))
            ((= (mod (+ time i) bus) 0)
             (setf i (+ i 1))
             (setf incr (lcm incr bus)))
            (t (setf time (+ time incr)))))))

(defparameter *part1*
  (multiple-value-bind (bus-id mins) (earliest-bus *timestamp* *schedule*)
    (* bus-id mins)))
(defparameter *part2* (earliest-timestamp *schedule*))
