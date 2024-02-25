(defpackage cl-training
  (:use :cl))
(in-package :cl-training)

(defparameter *log-path* #p"/home/max/projects/lisp/training/data/training.log")
(defparameter *program-path* #p"/home/max/projects/lisp/training/data/program.log")

(defun interactive-log-training ())
(defun interactive-log-exercise ())


(defun tonnage (exercise))
(defun max-1rm (exercise))