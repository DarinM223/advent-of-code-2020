(defpackage day20 (:use :cl :iterate :utils))
(in-package :day20)

(defun parse-tile (tile-chunk)
  (let* ((tile-label (car tile-chunk))
         (tile-label-right (cadr (uiop:split-string tile-label)))
         (tile-num-str (string-right-trim ":" tile-label-right))
         (tile-num (parse-integer tile-num-str))
         (rest (coerce (cdr tile-chunk) 'vector)))
    (list tile-num rest)))

(defparameter *input*
  (with-open-file (stream (relative-path #P"resources/day20.txt"))
    (mapcar #'parse-tile
            (split-list (iter (for line in-lines stream) (collect line))))))

(defun string-min (a b)
  (if (string< a b) a b))

(defun string-id (s)
  (string-min s (reverse s)))

(defun corners (grid)
  (let* ((width (length (aref grid 0)))
         (top (aref grid 0))
         (bottom (aref grid (- (length grid) 1)))
         (left (map 'string #'identity
                    (iter (for y from 0 below (length grid))
                      (collect (char (aref grid y) 0)))))
         (right (map 'string #'identity
                     (iter (for y from 0 below (length grid))
                       (collect (char (aref grid y) (- width 1)))))))
    (list top bottom left right)))

(defparameter *edge-table*
  (iter (with result = (make-hash-table :test #'equal))
    (for (tile-num tile-grid) in *input*)
    (iter (for corner in (corners tile-grid))
      (for sid = (string-id corner))
      (setf (gethash sid result) (cons tile-num (gethash sid result))))
    (finally (return result))))

(defparameter *edges*
  (iter (for (k ids) in-hashtable *edge-table*)
    (when (> (length ids) 1) (collect ids))))

(defun build-graph (ids edges)
  (let ((node-table (iter (for id in ids) (collect id => (fset:empty-set)))))
    (iter (for (a b) in edges)
      (setf (gethash a node-table) (fset:with (gethash a node-table) b))
      (setf (gethash b node-table) (fset:with (gethash b node-table) a))
      (finally  (return node-table)))))

(defparameter *part1*
  (let ((graph (build-graph (mapcar #'first *input*) *edges*)))
    (iter (for (id edges) in-hashtable graph)
      (when (= (fset:set-size edges) 2) (multiplying id)))))
