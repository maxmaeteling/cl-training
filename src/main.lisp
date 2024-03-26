(defpackage cl-training
  (:use :cl :cl-training.parsers :maxpc :cl-training.classes :cl-training.print :eazy-gnuplot))
(in-package :cl-training)

(declaim (optimize (debug 3)))

(defparameter *path* #p"/home/max/projects/lisp/cl-training/")

(defparameter *log-path* (merge-pathnames #p"data/training.log" *path*))
(defparameter *program-path*  (merge-pathnames #p"data/program.log" *path*))
(defparameter *alias-path* (merge-pathnames #p"data/aliases/" *path*))

(defparameter *images-path* (merge-pathnames #p"output/images/" *path*))

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

(defun filter-trainings-exercise-names (logbook name)
  (loop
	for training in logbook
	for exercises-filtered = (loop
							   for exercise in (training-exercises training)
							   when (string= name (exercise-name exercise))
								 collect exercise)
	when exercises-filtered
	  collect (make-training (training-date training)
							 exercises-filtered)))

(defun trainings-exercise-names (logbook)
  (loop
	for training in logbook
	for exercise-names = (mapcar #'exercise-name
								 (training-exercises training))
	append exercise-names))

(defun trainings-1rms (logbook)
  (loop
	for training in logbook
	collect (training-1rm training)))

(defun training-1rm (training)
  (make-training (training-date training)
				 (loop
				   for exercise in (training-exercises training)
				   collect (exercise-1rm exercise))))

(defmethod set>= ((set-1 set-weight) (set-2 set-weight))
  "Compare by weight"
  (if (>= (set-weight set-1)
		  (set-weight set-2))
	  set-1
	  set-2))

(defmethod set>= ((set-1 exercise-set) (set-2 exercise-set))
  "Compare by reps"
  (if (>= (set-reps set-1)
		  (set-reps set-2))
	  set-1
	  set-2))

(defun logbook-date-weight (logbook)
  (loop
	for training in logbook
	append (loop
			 for exercise in (training-exercises training)
			 collect (list (training-date training)
						   (set-max-effort (car (exercise-sets exercise)))))))

(defmethod set-max-effort ((set exercise-set))
  (set-reps set))

(defmethod set-max-effort ((set set-weight))
  (set-weight set)) 

(defun exercise-1rm (exercise)
  (make-exercise (exercise-name exercise)
				 (list (reduce #'set>= (mapcar #'set-1rm
												 (exercise-sets exercise))))))

(defmethod set-1rm ((set exercise-set))
  (make-exercise-set (reduce #'max (set-reps set))))

(defmethod set-1rm ((set set-weight))
  (make-set-weight 1
				   (float (1rm (set-weight set)
							   (set-reps set)))))

(defmethod set-1rm ((set multi-set-weight))
    (make-set-weight 1
					 (float (1rm (set-weight set)
								 (set-reps set)))))

(defmethod set-tonnage ((set exercise-set))
  (reduce #'+ (set-reps set)))

(defmethod set-tonnage ((set set-weight))
  (* (set-weight set)
	 (set-reps set)))

(defmethod set-tonnage ((set multi-set-weight))
  (* (set-number set)
	 (set-weight set)
	 (set-reps set)))