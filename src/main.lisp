(defpackage cl-training
  (:use :cl :cl-training.plots :cl-training.log :cl-training.print  :cl-training.classes :local-time))
(in-package :cl-training)

(defun regenerate-plots ()
  (ensure-alias-db)
  (let ((log (filter-log (normalize-exercise-names (load-parse-training))
						 :training #'(lambda (tr)
									   (timestamp< (adjust-timestamp (now)
																	 (offset :year -1))
												   (training-date tr))))))
	(let ((exercises '("Low Bar Squat"
					   "Deadlifts"
					   "Press"
					   "Bench Press")))
	  (mapcar #'(lambda (exercise)
				  (exercise-plot-time/1rm log
										  (string-downcase exercise)
										  exercise
										  (format nil "~a_1rm.png" (string-downcase exercise))))
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