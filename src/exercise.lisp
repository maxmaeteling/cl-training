(defpackage cl-training.exercise
  (:use :cl :cl-training.config)
  (:export
   #:def-exercise))

(in-package :cl-training.exercise)

(defun def-exercise (name &key (aliases '()) (base 0))
  (push (list name aliases base)  *exercises* ))
