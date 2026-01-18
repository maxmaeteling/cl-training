(defpackage cl-training.plots
  (:use :cl :eazy-gnuplot :local-time :cl-training.classes :cl-training.config
   :cl-training.parsers-new :cl-training.log-new)
  (:export
   :plot-time/values
   :exercise-plot-time/1rm
   :exercise-plot-time/1rms
   :exercise-plot-time/tonnage
   :exercise-plot-time/max-rm
   :exercise-plot-time/tonnages))

(in-package :cl-training.plots)

(defun output-image-path (name)
  (merge-pathnames name *images-path*))

(defun date-gnuplot (s date)
  (format-timestring s date :format '((:year 4) #\- (:month 2) #\- (:day 2))))

(defun plot-time/values (output title data-titles data &optional (debug nil))
  (with-plots (s :debug debug)
	(gp-setup :terminal '(png :size "1200,900") :output output)
	(gp :set :xdata 'time)
    (gp :set :timefmt "%Y-%m-%d")
	(gp :set :format '(x "%m/%y"))
	(gp :set :title title)
	(gp :set :datafile '(:missing "0.0"))
	(gp :set :grid :ytics)
	(gp :set :key :left :top)
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

(defun exercise-plot-time/max-rm (log exercise-name title file
								  &key (training #'(lambda (x) (declare (ignore x)) t))
									(exercise #'(lambda (x) (declare (ignore x)) t)))
  (exercise-plot-time/transform log #'identity (list exercise-name) (list title) title file
								:training training :exercise exercise))

(defun exercise-plot-time/1rm (log exercise-name title file
							   &key (training #'(lambda (x) (declare (ignore x)) t))
								 (exercise #'(lambda (x) (declare (ignore x)) t)))
  (exercise-plot-time/transform log #'trainings-1rms (list exercise-name) (list title) title file
								:training training :exercise exercise))

(defun exercise-plot-time/tonnage (log exercise-name title file
								   &key (training #'(lambda (x) (declare (ignore x)) t))
									 (exercise #'(lambda (x) (declare (ignore x)) t)))
  (exercise-plot-time/transform log #'trainings-tonnage (list exercise-name) (list title) title file
								:training training :exercise exercise))

(defun exercise-plot-time/max-rms (log exercise-names exercise-titles title file
								   &key (training #'(lambda (x) (declare (ignore x)) t))
									 (exercise #'(lambda (x) (declare (ignore x)) t)))
  (exercise-plot-time/transform log #'identity exercise-names exercise-titles title file
								:training training :exercise exercise))

(defun exercise-plot-time/1rms (log exercise-names exercise-titles title file
								&key (training #'(lambda (x) (declare (ignore x)) t))
								  (exercise #'(lambda (x) (declare (ignore x)) t)))
  (exercise-plot-time/transform log #'trainings-1rms exercise-names exercise-titles title file
								:training training :exercise exercise))

(defun exercise-plot-time/tonnages (log exercise-names exercise-titles title file
									&key (training #'(lambda (x) (declare (ignore x)) t))
									  (exercise #'(lambda (x) (declare (ignore x)) t)))
  (exercise-plot-time/transform log #'trainings-tonnage exercise-names exercise-titles title file
								:training training :exercise exercise))

(defun exercise-plot-time/transform (log fn-transform exercise-names exercise-titles title file
									 &key (training #'(lambda (x) (declare (ignore x)) t))
									   (exercise #'(lambda (x) (declare (ignore x)) t)))
  (plot-time/values
   (output-image-path file)
   title
   exercise-titles
   (columnify exercise-names
			  (funcall
			   fn-transform
			   (filter-log
				log
				:training training
				:exercise #'(lambda (ex) (and (funcall exercise ex)
											  (member (exercise-name ex)
													  exercise-names
													  :test #'string=))))))))

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
				 maximize (cl-training.log-new:set-max-effort set))))