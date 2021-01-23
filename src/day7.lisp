(defpackage day7 (:use :cl :iterate :utils))
(in-package :day7)

(defvar *input*
  (with-open-file (stream (relative-path #P"resources/day7.txt"))
    (iter (for line in-lines stream) (collect line))))

(defun list-chunks (l &key (size 3))
  (iter (for e in l)
        (for i first 1 then (+ i 1))
        (collect e into chunk)
        (when (= (mod i size) 0)
          (collect chunk)
          (setf chunk nil))))

(defun process-line (line)
  (let* ((words (mapcar (lambda (w) (string-trim ".," w))
                        (uiop:split-string line :separator " ")))
         (filtered (remove-if (lambda (w) (or (string= w "bag")
                                              (string= w "bags")))
                              words)))
    (destructuring-bind ((bagw1 bagw2) contains-bags)
        (split-list (append filtered '("contain")) :separator "contain")
      (values (concatenate 'string bagw1 bagw2)
              (iter (for (num w1 w2) in (list-chunks contains-bags))
                    (collect (list (parse-integer num)
                                   (concatenate 'string w1 w2))))))))

(defun make-bag-hashtable (input)
  (iter (with bag-map = (make-hash-table :test 'equal))
        (for line in input)
        (for (values bag contains-bags) = (process-line line))
        (setf (gethash bag bag-map) contains-bags)
        (finally (return bag-map))))

(defun contains-color (color test-color bag-map)
  (if (string= color test-color)
      t
      (iter (for (num c) in (gethash color bag-map))
            (thereis (contains-color c test-color bag-map)))))

(defun nested-colors (color bag-map)
  (if (gethash color bag-map)
      (iter (for (num c) in (gethash color bag-map))
            (summing (+ num (* num (nested-colors c bag-map)))))
      0))

(defparameter *part1*
  (let ((bag-map (make-bag-hashtable *input*)))
    (iter (for (color v) in-hashtable bag-map)
          (when (not (string= color "shinygold"))
            (counting (contains-color color "shinygold" bag-map))))))

(defparameter *part2*
  (nested-colors "shinygold" (make-bag-hashtable *input*)))
