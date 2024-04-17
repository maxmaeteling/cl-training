(defpackage cl-training.print
  (:use :cl :cl-training.classes :local-time)
  (:export :output-readable))
(in-package :cl-training.classes)

(defun output-date (stream date)
  (local-time:format-timestring stream
								date
								:format (list '(:year 4) #\/
											  '(:month 2) #\/
											  '(:day 2))))

(defmethod output-set-readable (stream (set exercise-set))
  (format stream "~{~d~^,~}" (set-reps set)))

(defmethod output-set-readable (stream (set set-weight))
  (format stream "~d*~f" (set-reps set) (set-weight set)))

(defmethod output-set-readable (stream (set multi-set-weight))
  (format stream "~d*~d*~f" (set-number set) (set-reps set) (set-weight set)))

(defun output-exercise-readable (stream exercise)
  (format stream
		  "~a: ~{~a ~}~%"
		  (exercise-name exercise)
		  (mapcar #'(lambda (set) (output-set-readable nil set)) 
				  (exercise-sets exercise))))

(defun output-readable (log &optional (stream nil))
  (loop
	for training in log
	do (progn (format stream "~&")
			  (output-date stream (training-date training))
			  (format stream "~&")
			  (mapcar #'(lambda (ex) (output-exercise-readable stream ex))
					  (training-exercises training))
			  (format stream "~%"))))

(defgeneric columnize (object)
  (:documentation "Representation of object as a list of fields")
  (:method ((o list)) o))

(defun transpose (lists)
  (when (notany #'null lists)
	(cons (mapcar #'first lists)
		  (transpose (mapcar #'cdr lists)))))

(defun tabulate (stream objects)
  (loop 
	for n from 0
	for o in objects
	for row = (mapcar #'princ-to-string (columnize o))
	collect row into rows 
	collect (mapcar #'length row) into row-widths 
	finally 
	   (flet ((build-format-arguments (max-width row) 
				(when (> max-width 0) 
				  (list max-width #\space row)))) 
		 (loop
		   with number-width = (ceiling (log n 10)) 
		   with col-widths = (transpose row-widths) 
		   with max-col-widths = (mapcar (lambda (s) (reduce #'max s)) col-widths)
		   for index from 0 
		   for row in rows
		   for entries = (mapcan #'build-format-arguments max-col-widths row) 
		   do (format stream
					  "~v,'0d. ~{~v,,,va~^ ~}~%" 
					  number-width index entries)))))