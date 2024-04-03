(defpackage cl-training.config
  (:use :cl)
  (:export
   #:*path*
   #:*log-path*
   #:*program-path*
   #:*alias-path*
   #:*images-path*
   #:*alias-db*))

(in-package :cl-training.config)

(defparameter *path* #p"/home/max/projects/lisp/cl-training/")

(defparameter *log-path* (merge-pathnames #p"data/training.log" *path*))
(defparameter *program-path* (merge-pathnames #p"data/program.log" *path*))
(defparameter *alias-path* (merge-pathnames #p"data/aliases" *path*))

(defparameter *images-path* (merge-pathnames #p"output/images/" *path*))

(defparameter *alias-db* nil)
