(defpackage cl-training
  (:use
   :cl
   :cl-training.plots
   :cl-training.log
   :cl-training.print
   :cl-training.classes
   :local-time))

(in-package :cl-training)

(defun regenerate-plots ()
  (ensure-alias-db)
  (let* ((log-unfiltered (normalize-exercise-names (load-parse-training)))
		 (log (filter-log log-unfiltered
						  :training #'(lambda (tr)
										(timestamp< (adjust-timestamp (now)
													  (offset :year -1))
													(training-date tr))))))
	(let ((exercises '("Low Bar Squat"
					   "Deadlifts"
					   "Press"
					   "Bench Press"
					   "Power Clean")))
	  (mapcar #'(lambda (exercise)
				  (exercise-plot-time/1rm log
										  (string-downcase exercise)
										  exercise
										  (format nil "~a_1rm_last_year.png" (string-downcase exercise))))
			  exercises)
	  (mapcar #'(lambda (exercise)
				  (exercise-plot-time/1rm log-unfiltered
										  (string-downcase exercise)
										  exercise
										  (format nil "~a_1rm_all_time.png" (string-downcase exercise))))
			  exercises)
	  (mapcar #'(lambda (exercise)
				  (exercise-plot-time/tonnage log
											  (string-downcase exercise)
											  exercise
											  (format nil "~a_tonnage.png" (string-downcase exercise))))
			  exercises)
	  (exercise-plot-time/1rms log
							   (mapcar #'string-downcase exercises)
							   exercises
							   "1rm Comparison"
							   "comparison_1rm.png")
	  (exercise-plot-time/tonnages log
								   (mapcar #'string-downcase exercises)
								   exercises
								   "Tonnage Comparison"
								   "comparison_tonnage.png"))))

(defun print-exercise-1rms (exercise-name)
  (ensure-alias-db)
  (output-readable
   (trainings-1rms
	(filter-log (normalize-exercise-names (load-parse-training))
				:exercise #'(lambda (ex) (string= exercise-name
												  (exercise-name ex)))))
   t))

(defun print-exercise-tonnage (exercise-name)
  (ensure-alias-db)
  (output-readable
   (trainings-tonnage
	(filter-log (normalize-exercise-names (load-parse-training))
				:exercise #'(lambda (ex) (string= exercise-name
												  (exercise-name ex)))))
   t))

(defun print-log ()
  (ensure-alias-db)
  (output-readable
   (normalize-exercise-names (load-parse-training))
   t))

(defun print-1rm-table (stream exercises)
  (let ((log (trainings-1rms
			  (filter-log (normalize-exercise-names (load-parse-training))
						  :exercise #'(lambda (ex) (member (exercise-name ex)
														   exercises :test #'string=))))))
	(format-table
	 stream
	 (mapcar #'(lambda
				   (training)
				 (cons
				  (output-date nil (training-date training))
				  (mapcar #'(lambda
								(exercise)
							  (format nil "~,2f"
									  (or
									   (loop
										 for training-exercise in (training-exercises training)
										 when (string= exercise
													   (exercise-name training-exercise))
										   do (return (set-weight (first (exercise-sets training-exercise)))))
									   0)))
						  exercises)))
			 log)
	 :column-label (mapcar #'string-upcase (cons "date" exercises))
	 :column-align (cons :left (loop repeat (length exercises) collect :right)))))

(defun max-reps (log)
  (let ((max-weights (make-array 21 :element-type 'float :initial-element 0.0)))
	(loop
	  for training in log
	  do (loop
		   for exercise in (training-exercises training)
		   do (loop
				for set in (exercise-sets exercise)
				do (when (and
						  (>= 20 (set-reps set))
						  (> (set-weight set) (aref max-weights (set-reps set))))
					 (setf (aref max-weights (set-reps set))
						   (set-weight set))))))
	max-weights))