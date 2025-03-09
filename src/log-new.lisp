(defpackage cl-training.log-new
  (:use :cl :cl-training.config :cl-training.classes :cl-training.parsers-new :local-time :maxpc :cl-training.exercise)
  (:export
   #:load-parse-training
   #:normalize-exercise-name
   #:normalize-exercise-names
   #:build-alias-hashtable
   #:load-parse-aliases
   #:create-alias-db
   #:filter-log
   #:trainings-1rms
   #:set-max-effort
   #:trainings-tonnage
   #:ensure-alias-db
   #:log-sets))

(in-package :cl-training.log-new)

(defun load-parse-training (&optional (path *log-path*))
  (process-log-expr
   (with-open-file (s path)
	 (first (parse s (=trainings))))))

(defun build-alias-hashtable (&optional (exercises *exercises*))
  (loop
	for (word aliases . _) in exercises
	with alias-hash = (make-hash-table :test #'equalp)
	do (setf (gethash word alias-hash) word)
	do (mapcar #'(lambda (alias)
				   (setf (gethash alias alias-hash) word))
			   aliases)
	finally (return alias-hash)))

(defun normalize-exercise-name (name &optional (db *alias-db*))
  (multiple-value-bind (ret found) (gethash name db)
	(if found
		ret
		(format nil "NOT NORMALIZED ~a" name))))

(defun process-sets (sets)
  sets)

(defun exercise-info (name &optional (exercises *exercises*))
  (find name exercises :key #'first :test #'string=))

(defun expr-value (expr)
  (declare (ignore expr))
  97)

(defun process-set-symbol (set-expr base)
  (ecase (length set-expr)
	(1 (list (make-multi-set-weight (list 1) (first set-expr) (list (expr-value base)))))
	(2 (list (make-multi-set-weight (first set-expr) (second set-expr) (list (expr-value base)))))
	(3 (list (make-multi-set-weight (first set-expr)
									(second set-expr)
									(mapcar #'(lambda (x) (+ x (expr-value base)))
											(third set-expr)))))))

(defun process-set-numeric (set-expr base)
  (ecase (length set-expr)
	(1 (list (make-multi-set-weight '(1) '(1) (first set-expr))))
	(2 (list (make-multi-set-weight '(1)
									(first set-expr)
									(second set-expr))))
	(3 (list (make-multi-set-weight (first set-expr)
									(second set-expr)
									(mapcar #'(lambda (x) (+ x base))
											(third set-expr)))))))

(defun process-set-expr (name set)
  (let ((info (exercise-info name)))
	(if (not info)
		(process-set-numeric set 0)
		(destructuring-bind (name aliases base)
			info
		  (declare (ignore name aliases))
		  (if (symbolp base)
			  (process-set-symbol set base)
			  (process-set-numeric set base))))))

(defun process-exercise-expr (exercise-expr)
  (ensure-alias-db)
  (destructuring-bind (name sets)
	  exercise-expr
	(let ((name-normalized (normalize-exercise-name name)))
	  (make-exercise name-normalized
					 (normalize-sets
					  (reduce #'append
							  (mapcar #'(lambda (set)
										  (process-set-expr name-normalized set))
									  sets)))))))

(defun process-log-expr (&optional (log (load-parse-training)))
  (loop
	for (date exercises) in log
	collect (make-training date (mapcar #'process-exercise-expr exercises))))

(defun normalize-sets (sets)
  (loop for set in sets
		append
		(loop
		  for i in (set-number set)
		  append (loop
				   for j in (set-reps set)
				   append (loop for k in (set-weight set)
								collect (make-multi-set-weight i j k))))))

(defun ensure-alias-db ()
  (when (not *alias-db*)
	(create-alias-db)))

(defun create-alias-db ()
  (setf *alias-db* (build-alias-hashtable)))

(defun 1rm (weight reps)
  "Calculate 1 repetition max based on weights anc reps according to Brzycki formula from https://en.wikipedia.org/wiki/One-repetition_maximum"
  (if (< reps 10)
	  (/ weight (- (/ 37 36) (/ reps 36)))
	  (/ weight (- (/ 37 36) (/ 10 36)))))

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

(defmethod training-index ((training training))
  (training-date training))

(defun log-sets (log)
  (let ((training-dates (make-array 1024 :fill-pointer 0))
		(exercise-names (make-array 64 :fill-pointer 0))
		(sets (make-array '(4096 3) :adjustable t)))
	(loop
	  for training in log
	  with i = 0
	  do (progn
		   (vector-push (training-index training) training-dates)
		   (loop
			 for exercise in (training-exercises training)
			 do (vector-push (exercise-name exercise) exercise-names)
			 do (loop
				  for set in (exercise-sets exercise)
				  do (progn (setf (aref sets i 0) (1- (fill-pointer training-dates))
								  (aref sets i 1) (1- (fill-pointer exercise-names))
								  (aref sets i 2) set)
							(incf i))))))
	(values training-dates exercise-names sets)))