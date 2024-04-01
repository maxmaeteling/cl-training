(defpackage cl-training.plots
  (:use :cl :eazy-gnuplot :local-time))
(in-package :cl-training.plots)

(defun date-gnuplot (s date)
  (format-timestring s date :format '((:year 4) #\- (:month 2) #\- (:day 2))))

(defun plot-time/value (output title data)
  (with-plots (s :debug nil)
	(gp-setup :terminal '(png :size "1200,900") :output output)
	(gp :set :xdata 'time)
    (gp :set :timefmt "%Y-%m-%d")
	(gp :set :format '(x "%m/%y"))
	(gp :set :title title)
	(plot
	 (lambda ()
	   (loop
		 for (date value) in data
		 for date-gp = (date-gnuplot nil date)
		 do (format s "~&~a ~a" date-gp (float value))))
	 :using '(1 2)
	 :with '(:points :pt 7))
	output))

(defun exercise-plot-time/1rm (exercise-name title file
							   &key (training #'(lambda (x) (declare (ignore x)) t))
								 (exercise #'(lambda (x) (declare (ignore x)) t)))
  (plot-time/value
   (output-image-path file)
   title
   (logbook-date-weight
	(filter-log (trainings-1rms (normalize-exercise-names (load-parse-training)))
				:training training
				:exercise #'(lambda (e) (and (funcall exercise e)
											 (string= (exercise-name e)
													  exercise-name)))))))

(defun plot-time/values (output title data-titles data)
  (with-plots (s :debug nil)
	(gp-setup :terminal '(png :size "1200,900") :output output)
	(gp :set :xdata 'time)
    (gp :set :timefmt "%Y-%m-%d")
	(gp :set :format '(x "%m/%y"))
	(gp :set :title title)
	(flet ((data ()
			 (loop
			   for (date . values) in data
			   for date-gp = (date-gnuplot nil date)
			   do (format s "~&~a ~{~a~^ ~}" date-gp (mapcar #'float values)))))
	  (loop
		for i from 2 to (length (car data))
		do (plot
			#'data
			:using (list 1 i) :title (nth (- i 2) data-titles)
			:with '(:points :pt 7))))
	output))

(defun exercise-plot-time/1rms (exercise-names title file)
  (plot-time/values
   (output-image-path file)
   title
   exercise-names
   (columnify exercise-names
			  (trainings-1rms
			   (normalize-exercise-names
				(load-parse-training))))))

(defun columnify (exercise-names logbook)
  (loop
	for training in logbook
	for results = (loop
					for name in exercise-names
					collect (exercises-exercise-max name (training-exercises training)))
	collect (cons (training-date training) results)))

(defun exercises-exercise-max (name exercises)
  (loop
	for exercise in exercises
	when (string= name (exercise-name exercise))
	maximize (loop
			   for set in (exercise-sets exercise)
			   maximize (set-max-effort set))))

(defun exercise-plot-time/tonnage (exercise title file)
  (plot-time/value
   (output-image-path file)
   title
   (logbook-date-weight
	(trainings-tonnage (filter-log (normalize-exercise-names (load-parse-training))
								   :exercise #'(lambda (e) (string= (exercise-name e)
																	exercise)))))))