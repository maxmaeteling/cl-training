(defpackage cl-training
  (:use
   :cl
   :cl-training.config
   :cl-training.plots
   :cl-training.log-new
   :cl-training.print
   :cl-training.classes
   :cl-training.helpers
   :local-time))

(in-package :cl-training)

(defun regenerate-plots ()
  (let* ((log-unfiltered (read-parse-log))
		 (log (filter-log log-unfiltered
						  :training #'(lambda (tr)
										(timestamp< (adjust-timestamp (now)
													  (offset :year -1))
													(training-date tr))))))
	(let ((exercises '("Low Bar Squat"
					   "Deadlift"
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
  (output-readable
   (trainings-1rms
	(filter-log (read-parse-log)
				:exercise #'(lambda (ex) (string= exercise-name
												  (exercise-name ex)))))
   t))

(defun print-exercise-tonnage (exercise-name)
  (output-readable
   (trainings-tonnage
	(filter-log (read-parse-log)
				:exercise #'(lambda (ex) (string= exercise-name
												  (exercise-name ex)))))
   t))

(defun print-weekly-tonnage (exercise-name)
  (format-table t
				(hash-table-to-list
				 (collate 
				  (trainings-tonnage 
				   (filter-log (read-parse-log)
							   :exercise #'(lambda (ex)
											 (string= exercise-name
													  (exercise-name ex)))))
				  :key #'(lambda (training)
						   (format-timestring nil (training-date training)
											  :format '(:year "-" :iso-week-number)))
				  :value #'(lambda (training)
							 (weight (first (exercise-sets (first (training-exercises training))))))
				  :merger #'+
				  :test #'equal
				  :default 0))
				:column-label '("Week" "Tonnage")
				:column-align '(:left :right)))

(defun flatten-log (log)
  (loop
	for training in log
	append (loop
			 for exercise in (training-exercises training)
			 append (loop
					  for exercise-set in (exercise-sets exercise)
					  collect (list (training-date training)
									(exercise-name exercise)
									exercise-set)))))

(defun exercise-max-reps-all (log)
  (collate (flatten-log log)
		   :key #'(lambda (set-expr)
					(list (second set-expr)
						  (reps (third set-expr))))
		   :test #'equalp
		   :merger #'(lambda (a b)
					   (cond ((not a) b)
							 ((or (not b)
								  (> (weight (third a))
									 (weight (third b)))
								  (and (= (weight (third a))
										  (weight (third b)))
									   (timestamp> (first a)
												   (first b))))
							  a)
							 (t b)))))

(defun exercise-last-date (log)
  (collate (flatten-log log)
		   :key #'second
		   :value #'first
		   :test #'equalp
		   :merger #'(lambda (a b) (if (timestamp> a b) a b))
		   :default (unix-to-timestamp 0)))

(defun org-report (&optional (stream nil) (log (read-parse-log)))
  (labels ((exercise-detail (ex s exercise-last-dates exercise-max-reps)
			 (format s "Last training: ~a (~d week(s) ago)~%~%"
					 (timestamp-short-date nil (gethash ex exercise-last-dates))
					 (timestamp-whole-week-difference (now)
													  (gethash ex exercise-last-dates)))
			 (format s "**** Max reps~%")
			 (format s "| RM | Weight | Date |~%")
			 (loop
			   for n from 1 to 10
			   for ex-max = (gethash (list ex n) exercise-max-reps)
			   when ex-max
				 do (format s "|  ~a | ~a | ~a |~%"
							n
							(read-weight (third ex-max))
							(timestamp-short-date nil (first ex-max))))
			 (format s "~%")))
	(let ((exercise-names (remove-duplicates
						   (sort (copy-seq (mapcar #'second (flatten-log log)))
								 #'string<)
						   :test #'string=))
		  (exercise-last-dates (exercise-last-date log))
		  (exercise-max-reps (exercise-max-reps-all log)))
	  (princ 
	   (with-output-to-string (s)
		 (format s "* Training report~%")
		 
		 (format s "** Exercises alphabetic~%")
		 (loop
		   for ex in exercise-names
		   do (progn
				(format s "*** ~a~%" (string-capitalize ex))
				(exercise-detail ex s exercise-last-dates exercise-max-reps)))

		 (format s "** Exercises recency~%")
		 (loop
		   with exercises-recency = (sort (hash-table-to-list exercise-last-dates)
										  #'timestamp>
										  :key #'second)
		   for (ex date) in exercises-recency
		   do (progn
				(format s "*** ~a (~d week(s) ago) ~%"
						(string-capitalize ex)
						(timestamp-whole-week-difference (now) date))
				(exercise-detail ex s exercise-last-dates exercise-max-reps)))

		 (format s "** Last half year~%")
		 (output-readable (filter-log log
									  :training #'(lambda (tr)
													(timestamp< (adjust-timestamp (now)
																  (offset :month -6))
																(training-date tr))))
						  s
						  3))
	   stream))))

(defun org-report-to-file (&optional (path *org-report-path*))
  (with-open-file (output-stream path
								 :direction :output
								 :if-does-not-exist :create
								 :if-exists :overwrite)
	(org-report output-stream)))

(defun exercise-names-counts (log)
  (loop
	with count = (make-hash-table :test #'equalp)
	for training in log
	do (loop for exercise in (training-exercises training)
			 do (incf (gethash (exercise-name exercise) count 0)))
	finally (return (hash-table-to-list count))))

(defun print-log ()
  (output-readable
   (read-parse-log)
   t))

(defun print-1rm-table (stream exercises)
  (let ((log (trainings-1rms
			  (filter-log (read-parse-log)
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
										   do (return (weight (first (exercise-sets training-exercise)))))
									   0)))
						  exercises)))
			 log)
	 :column-label (mapcar #'string-upcase (cons "date" exercises))
	 :column-align (cons :left (loop repeat (length exercises) collect :right)))))

(defun max-reps (log)
  "Calculate a max rep list for logbook"
  (let ((max-weights (make-array 20 :element-type 'float :initial-element 0.0)))
	(loop
	  for training in log
	  do (loop
		   for exercise in (training-exercises training)
		   do (loop
				for set in (exercise-sets exercise)
				do (loop
					 for i from 1 to (min 20 (reps set))
					 when (< (aref max-weights (1- i))
							 (weight set))
					   do (setf (aref max-weights (1- i))
								(weight set))))))
	max-weights))

(defun max-reps-exercises (log exercises)
  (loop
	for exercise-name in exercises
	collect (max-reps log)))

(defun print-max-reps (stream log names)
  "Print table of maximum weight by reps (1,2,...)"
  (loop
	for name in (listify names)
	collect (mapcar #'(lambda (x) (format nil "~,2f" x))
					(coerce (exercise-max-reps log name) 'list))
	  into maxes
	collect name into labels
	collect :right into alignments
	finally (format-table
			 stream
			 (transpose (cons (loop for i from 1 for j on (first maxes) collect i)
							  maxes))
			 :column-label (cons "reps" labels)
			 :column-align (cons :right alignments))))

(defun print-max-reps-default (stream names)
  "Print table of maximum weight with default file"
  (let ((log (read-parse-log)))
	(print-max-reps stream log names)))

(defun exercise-max-reps (log name)
  "Load default data and create a max rep weight list for one exercise"
  (max-reps (filter-log log
						:exercise #'(lambda (ex) (string= name (exercise-name ex))))))