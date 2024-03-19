(defpackage cl-training.print
  (:use :cl :cl-training.classes)
  (:export :output-readable))
(in-package :cl-training.classes)

(defun output-date (stream date-list)
  (format stream "~{~2,'0d~^/~}~%" date-list))

(defmethod output-set-readable (stream set)
  (break)
  (format stream "skipped"))

(defmethod output-set-readable (stream (set exercise-set))
  (format stream "~{~d~^,~}" (set-reps set)))

(defmethod output-set-readable (stream (set set-weight))
  (format stream "~dx~d" (set-reps set) (set-weight set)))

(defmethod output-set-readable (stream (set multi-set-weight))
  (format stream "~dx~dx~a" (set-number set) (set-reps set) (set-weight set)))

(defun output-exercise-readable (stream exercise)
  (format stream "~a: " (exercise-name exercise))
  (format stream "~{~a ~}~%" (mapcar #'(lambda (set) (output-set-readable nil set)) 
								   (exercise-sets exercise))))

(defun output-readable (log &optional (stream nil))
  (loop
	for training in log
	do (output-date stream (training-date training))
	do (mapcar #'(lambda (ex) (output-exercise-readable stream ex))
			   (training-exercises training))
	do (format stream "~%")))