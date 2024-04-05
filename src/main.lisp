(defpackage cl-training
  (:use :cl :cl-training.plots :cl-training.log :cl-training.print  :cl-training.classes))
(in-package :cl-training)

(defun regenerate-plots ()
  (create-alias-db)
  (exercise-plot-time/1rm "deadlifts" "Deadlifts" "deadlifts_1rm.png")
  (exercise-plot-time/1rm "low bar squat" "Squats" "squats_1rm.png")
  (exercise-plot-time/1rm "press" "Press" "press_1rm.png")
  (exercise-plot-time/1rm "bench press" "Bench Press" "bench_1rm.png")
  (let ((exercises '("Low Bar Squat"
					 "Deadlifts"
					 "Press"
					 "Bench Press")))
	(exercise-plot-time/1rms (mapcar #'string-downcase exercises)
							 exercises
							 "1rm Comparison"
							 "comparison_1rm.png")))

(defun print-exercise-1rms (exercise-name)
  (output-readable
   (trainings-1rms
	(filter-log (normalize-exercise-names (load-parse-training))
				:exercise #'(lambda (ex) (string= exercise-name
												  (exercise-name ex)))))
   t))