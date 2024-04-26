(defpackage cl-training.print
  (:use :cl :cl-training.classes :local-time)
  (:export
   :output-date
   :output-readable
   :format-table))

(in-package :cl-training.print)

(defconstant +CELL-FORMATS+ '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

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

;;;; source: https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f
(defun format-table (stream data &key (column-label (loop for i from 1 to (length (car data))
                                                          collect (format nil "COL~D" i)))
                                   (column-align (loop for i from 1 to (length (car data))
                                                       collect :left)))
  (let* ((col-count (length column-label))
         (strtable (cons column-label	; table header
                         (loop for row in data ; table body with all cells as strings
							   collect (loop for cell in row
											 collect (if (stringp cell)
                                                         cell
										;else
                                                         (format nil "~A" cell))))))
         (col-widths (loop with widths = (make-array col-count :initial-element 0)
                           for row in strtable
                           do (loop for cell in row
                                    for i from 0
									do (setf (aref widths i)
											 (max (aref widths i) (length cell))))
                           finally (return widths))))
										;------------------------------------------------------------------------------------
										; splice in the header separator
    (setq strtable
          (nconc (list (car strtable)				   ; table header
                       (loop for align in column-align ; generate separator
                             for width across col-widths
                             collect (case align
                                       (:left   (format nil ":~v@{~A~:*~}"
                                                        (1- width)  "-"))
                                       (:right  (format nil "~v@{~A~:*~}:"
                                                        (1- width)  "-"))
                                       (:center (format nil ":~v@{~A~:*~}:"
                                                        (- width 2) "-")))))
                 (cdr strtable)))		; table body
										;------------------------------------------------------------------------------------
										; Generate the formatted table
    (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%" ; compile the row format
                           (loop for align in column-align
								 collect (getf +CELL-FORMATS+ align))))
          (widths  (loop for w across col-widths collect w)))
										; write each line to the given stream
      (dolist (row strtable)
        (apply #'format stream row-fmt (mapcan #'list widths row))))))
