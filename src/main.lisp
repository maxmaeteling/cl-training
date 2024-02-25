(defpackage cl-training
  (:use :cl :cl-training.parsers :maxpc))
(in-package :cl-training)

(defparameter *log-path* #p"/home/max/projects/lisp/training/data/training.log")
(defparameter *program-path* #p"/home/max/projects/lisp/training/data/program.log")

(defun load-parse-training (&optional (path *log-path*))
  (with-open-file (s path)
	(parse s (=trainings))))

(defun interactive-log-training ())
(defun interactive-log-exercise ())

(defun 1rm (weight reps)
  "Calculate 1 repetition max based on weights anc reps according to Brzycki formula from https://en.wikipedia.org/wiki/One-repetition_maximum"
  (/ weight (- (/ 37 36) (/ reps 36))))

(defun tonnage (sets reps weight)
  (* sets reps weight))

(defun max-1rm (exercise))