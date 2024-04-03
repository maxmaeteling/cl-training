(defpackage cl-training.log
  (:use :cl :cl-training.config :cl-training.classes :cl-training.parsers :local-time :maxpc)
  (:export
   #:load-parse-training
   #:normalize-exercise-name
   #:normalize-exercise-names
   #:build-alias-hashtable
   #:load-parse-aliases
   #:create-alias-db
   #:filter-log
   #:trainings-1rms
   #:set-max-effort))

(in-package :cl-training.log)

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

(defun filter-log (log &key (training #'(lambda (x) (declare (ignore x)) t))
					 (exercise #'(lambda (x) (declare (ignore x)) t)))
  (loop
	for tr in log
	for exercises-filtered = (loop
							   for ex in (training-exercises tr)
							   when (funcall exercise ex)
								 collect ex)
	when (and (funcall training tr)
			  exercises-filtered)
	  collect (make-training (training-date tr)
							 exercises-filtered)))

(defun trainings-exercise-names (logbook)
  (loop
	for training in logbook
	for exercise-names = (mapcar #'exercise-name
								 (training-exercises training))
	append exercise-names))

(defun trainings-1rms (logbook)
  (mapcar #'training-1rm logbook))

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
				   (1rm (set-weight set)
						(set-reps set))))

(defmethod set-1rm ((set multi-set-weight))
    (make-set-weight 1
					 (1rm (set-weight set)
						  (set-reps set))))

(defun trainings-tonnage (log)
  (mapcar #'training-tonnage log))

(defun training-tonnage (training)
  (make-training (training-date training)
				 (mapcar #'exercise-tonnage (training-exercises training))))

(defun exercise-tonnage (exercise)
  (let ((sets (exercise-sets exercise)))
	(make-exercise (exercise-name exercise)
				   (list (make-set-weight
						  1
						  (reduce #'+ (mapcar #'(lambda (set)
												  (set-weight
												   (set-tonnage set))) 
											  sets)))))))

(defmethod set-tonnage ((set set-weight))
  (make-set-weight 1 (* (set-weight set)
						(set-reps set))))

(defmethod set-tonnage ((set multi-set-weight))
  (make-set-weight 1 (* (set-number set)
						(set-weight set)
						(set-reps set))))