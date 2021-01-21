(defpackage day4 (:use :cl :iterate :utils))
(in-package :day4)

(defun split-list (l)
  (iter (with start = 0)
        (for str in-sequence l with-index i)
        (when (string= str "")
          (progn
            (collect (subseq l start i))
            (setf start (+ i 1))))))

(defvar *input*
  (with-open-file (stream #P"../resources/day4.txt")
    (split-list (iter (for line in-lines stream) (collect line)))))

(defun add-fields (lines)
  (iter outer
        (with fields-map = (make-hash-table :test 'equal))
        (for line in lines)
        (iter (for field in (uiop:split-string line))
              (for (key value) = (uiop:split-string field :separator '(#\:)))
              (setf (gethash key fields-map) value))
        (finally (return-from outer fields-map))))

(defun contains-fields (field-map required-fields)
  (iter (for field in required-fields)
        (multiple-value-bind (v present) (gethash field field-map)
          (declare (ignore v))
          (always present))))

(defun validate-byr (s)
  (handler-case (let ((year (parse-integer s)))
                  (and (>= year 1920) (<= year 2002)))
    (t nil)))

(defun validate-iyr (s)
  (handler-case (let ((year (parse-integer s)))
                  (and (>= year 2010) (<= year 2020)))
    (t nil)))

(defun validate-eyr (s)
  (handler-case (let ((year (parse-integer s)))
                  (and (>= year 2020) (<= year 2030)))
    (t nil)))

(defun check-last-two-chars (s ch1 ch2)
  (and (char= (char s (- (length s) 1)) ch2)
       (char= (char s (- (length s) 2)) ch1)))

(defun validate-hgt (s)
  (handler-case
      (cond ((check-last-two-chars s #\c #\m)
             (let ((num (parse-integer (string-right-trim "cm" s))))
               (and (>= num 150) (<= num 193))))
            ((check-last-two-chars s #\i #\n)
             (let ((num (parse-integer (string-right-trim "in" s))))
               (and (>= num 59) (<= num 76)))))
    (t nil)))

(defun validate-hcl (s)
  (and
   (char= (char s 0) #\#)
   (= (length s) 7)
   (iter (for ch in-string s from 1)
         (for code = (char-code ch))
         (always (or (and (>= code (char-code #\0)) (<= code (char-code #\9)))
                     (and (>= code (char-code #\a)) (<= code (char-code #\f))))))))

(defun validate-ecl (s)
  (iter (for valid-clr in '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
        (thereis (string= s valid-clr))))

(defun validate-pid (s)
  (and (= (length s) 9)
       (iter (for ch in-string s) (always (digit-char-p ch)))))

(defun validate-fields (field-map required-fields)
  (iter (for (field validate-fn) in required-fields)
        (multiple-value-bind (v present) (gethash field field-map)
          (always (and present (funcall validate-fn v))))))

(defparameter *part1*
  (let ((required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))
    (iter (for lines in *input*)
          (counting (contains-fields (add-fields lines) required-fields)))))

(defparameter *part2*
  (let ((required-fields (list (list "byr" #'validate-byr)
                               (list "iyr" #'validate-iyr)
                               (list "eyr" #'validate-eyr)
                               (list "hgt" #'validate-hgt)
                               (list "hcl" #'validate-hcl)
                               (list "ecl" #'validate-ecl)
                               (list "pid" #'validate-pid))))
    (iter (for lines in *input*)
          (counting (validate-fields (add-fields lines) required-fields)))))
