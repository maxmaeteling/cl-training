(defpackage cl-training
  (:use :cl :cl-training.parsers :maxpc :cl-training.classes))
(in-package :cl-training)

(declaim (optimize (debug 3)))

(defparameter *log-path* #p"/home/max/projects/lisp/training/data/training.log")
(defparameter *program-path* #p"/home/max/projects/lisp/training/data/program.log")
(defparameter *alias-path* #p"/home/max/projects/lisp/training/data/aliases")

(defun load-parse-training (&optional (path *log-path*))
  (with-open-file (s path)
	(first (parse s (=trainings)))))

(defun load-parse-aliases (&optional (path *alias-path*))
  (with-open-file (s path)
	(first (parse s (=exercise-alias-lists)))))

(defun build-alias-hashtable (&optional (alias-list (load-parse-aliases)))
  (loop
	for aliases in alias-list
	for word = (car aliases)
	with alias-hash = (make-hash-table :test #'equalp)
	do (mapcar #'(lambda (alias)
			   (setf (gethash alias alias-hash) word))
		   aliases)
	finally (return alias-hash)))

(defun interactive-log-training ())
(defun interactive-log-exercise ())

(defun 1rm (weight reps)
  "Calculate 1 repetition max based on weights anc reps according to Brzycki formula from https://en.wikipedia.org/wiki/One-repetition_maximum"
  (/ weight (- (/ 37 36) (/ reps 36))))

(defun tonnage (sets reps weight)
  (* sets reps weight))

;; (defun max-1rm (exercise))

(defun filter-exercises (logbook name)
  (loop
	for training in logbook
	for exercise-names = (mapcar #'exercise-name
								 (training-exercises training))
	when (find name exercise-names :test #'string=) 
	  collect training))

(defun trainings-exercise-names (logbook)
  (loop
	for training in logbook
	for exercise-names = (mapcar #'exercise-name
								 (training-exercises training))
	append exercise-names))