(defpackage cl-training.classes
  (:use :cl :maxpc)
  (:export
   :training
   :make-training
   :exercise
   :make-exercise
   :exercise-set
   :make-exercise-set
   :set-weight
   :make-set-weight
   :multi-set-weight
   :make-multi-set-weight
   :multiply-sets
   :training-date
   :training-exercises
   :exercise-name
   :exercise-sets
   :set-reps
   :set-number
   :output-readable))
(in-package :cl-training.classes)

(defclass training ()
  ((date :initarg :date :accessor training-date)
   (exercises :initarg :exercises :accessor training-exercises)))

(defun make-training (date exercises)
  (make-instance 'training :date date :exercises exercises))

(defmethod print-object ((obj training) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((date training-date)
                     (exercises training-exercises))
        obj
      (format stream "~a, exercises: ~a" date exercises))))

(defclass exercise ()
  ((name :initarg :name :accessor exercise-name)
   (sets :initarg :sets :accessor exercise-sets)))

(defun make-exercise (name sets)
  (make-instance 'exercise :name name :sets sets))

(defmethod print-object ((obj exercise) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name exercise-name)
                     (sets exercise-sets))
        obj
      (format stream "~a: ~a" name sets))))

(defclass exercise-set ()
  ((reps :initarg :reps :accessor set-reps)))

(defun make-exercise-set (reps)
  (make-instance 'exercise-set :reps reps))

(defmethod print-object ((obj exercise-set) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((reps set-reps))
        obj
      (format stream "~a" reps))))

(defclass set-weight (exercise-set)
  ((weight :initarg :weight :accessor set-weight)))

(defmethod print-object ((obj set-weight) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((reps set-reps)
					 (weight set-weight))
        obj
      (format stream "~a*~a" reps weight))))

(defun make-set-weight (reps weight)
  (make-instance 'set-weight :reps reps :weight weight))

(defclass multi-set-weight (set-weight)
  ((set-number :initarg :number :accessor set-number)))

(defmethod print-object ((obj multi-set-weight) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((number set-number)
					 (reps set-reps)
					 (weight set-weight))
        obj
      (format stream "~a*~a*~a" number reps weight))))

(defun make-multi-set-weight (number reps weight)
  (make-instance 'multi-set-weight :number number :reps reps :weight weight))

(defun multiply-sets (numbers set-weights)
  (loop
	for n in numbers
	append (loop
			 for set-weight in set-weights
			 for reps = (set-reps set-weight)
			 for weight = (set-weight set-weight)
			 collect (make-multi-set-weight n reps weight))))