(defpackage cl-training
  (:use :cl :cl-training.parsers :maxpc :cl-training.classes :cl-training.print))
(in-package :cl-training)

(declaim (optimize (debug 3)))

(defparameter *log-path* #p"/home/max/projects/lisp/cl-training/data/training.log")
(defparameter *program-path* #p"/home/max/projects/lisp/cl-training/data/program.log")
(defparameter *alias-path* #p"/home/max/projects/lisp/cl-training/data/aliases")

(defparameter *alias-db* nil)

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

(defun normalize-exercise-name (name &optional (db *alias-db*))
  (gethash name db))

(defun create-alias-db ()
  (setf *alias-db* (build-alias-hashtable)))

(defun interactive-log-training ())
(defun interactive-log-exercise ())

(defun normalize-exercise-names (log)
  (loop
	for training in log
	do (loop
		 for exercise in (training-exercises training)
		 do (setf (exercise-name exercise)
				  (normalize-exercise-name (exercise-name exercise))))
	finally (return log)))

(defun 1rm (weight reps)
  "Calculate 1 repetition max based on weights anc reps according to Brzycki formula from https://en.wikipedia.org/wiki/One-repetition_maximum"
  (/ weight (- (/ 37 36) (/ reps 36))))

(defun tonnage (sets reps weight)
  (* sets reps weight))

(defun filter-training-by-exercises (logbook name)
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

(defun exercise-1rm (exercise)
  (reduce #'max
		  (mapcar #'set-1rm
				  (exercise-sets exercise))))

(defmethod set-1rm ((set exercise-set))
  (reduce #'max (set-reps set)))

(defmethod set-1rm ((set set-weight))
  (1rm (set-weight set)
	   (set-reps set)))

(defmethod set-1rm ((set multi-set-weight))
  (1rm (set-weight set)
	   (set-reps set)))

(defmethod set-tonnage ((set exercise-set))
  (reduce #'+ (set-reps set)))

(defmethod set-tonnage ((set set-weight))
  (* (set-weight set)
	 (set-reps set)))

(defmethod set-tonnage ((set multi-set-weight))
  (* (set-number set)
	 (set-weight set)
	 (set-reps set)))

