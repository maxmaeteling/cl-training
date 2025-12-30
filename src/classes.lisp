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
   :reps
   :num
   :weight
   :read-weight))

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
      (format stream "~&~a, ~&~a" date exercises))))

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
  ((reps :initarg :reps :accessor reps)))

(defun make-exercise-set (reps)
  (make-instance 'exercise-set :reps reps))

(defmethod print-object ((obj exercise-set) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((reps reps))
        obj
      (format stream "~a" reps))))

(defclass set-weight (exercise-set)
  ((weight :initarg :weight :accessor weight)))

(defmethod read-weight (set)
  0)

(defmethod print-object ((obj set-weight) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((reps reps)
					 (weight weight))
        obj
      (format stream "~a*~a" reps weight))))

(defmethod read-weight ((set set-weight))
  (weight set))

(defun make-set-weight (reps weight)
  (make-instance 'set-weight :reps reps :weight weight))

(defclass multi-set-weight (set-weight)
  ((num :initarg :num :accessor num)))

(defun make-multi-set-weight (number reps weight)
  (make-instance 'multi-set-weight :num number :reps reps :weight weight))

(defmethod print-object ((obj multi-set-weight) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((num num)
					 (reps reps)
					 (weight weight))
        obj
      (format stream "~a*~a*~a" num reps weight))))

(defun multiply-sets (numbers set-weights)
  (loop
	for n in numbers
	append (loop
			 for set-weight in set-weights
			 for reps = (reps set-weight)
			 for weight = (weight set-weight)
			 collect (make-multi-set-weight n reps weight))))