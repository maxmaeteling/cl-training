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
  (format stream "~d*~a" (set-reps set) (set-weight set)))

(defmethod output-set-readable (stream (set multi-set-weight))
  (format stream "~d*~d*~a" (set-number set) (set-reps set) (set-weight set)))

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