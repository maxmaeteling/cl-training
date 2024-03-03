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
   :make-multi-set-weight))
(in-package :cl-training.classes)

(defclass training ()
  ((date :initarg :date)
   (exercises :initarg :exercises)))

(defun make-training (date exercises)
  (make-instance 'training :date date :exercises exercises))

(defclass exercise ()
  ((name :initarg :name)
   (sets :initarg :sets)))

(defun make-exercise (name sets)
  (make-instance 'exercise :name name :sets sets))

(defclass exercise-set ()
  ((reps :initarg :reps)))

(defun make-exercise-set (reps)
  (make-instance 'exercise-set :reps reps))

(defclass set-weight (set)
  ((weight :initarg :weight)))

(defun make-set-weight (reps weight)
  (make-instance :set-weight :reps reps :weight weight))

(defclass multi-set-weight (set-weight)
  ((number :initarg :number)))

(defun make-multi-set-weight (number reps weight)
  (make-instance :number number :set-weight :reps reps :weight weight))
