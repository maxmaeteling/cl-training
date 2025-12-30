(defpackage cl-training.config
  (:use :cl)
  (:export
   #:*path*
   #:*log-path*
   #:*images-path*
   #:*org-report-path*))

(in-package :cl-training.config)

(defparameter *path* #p"/home/max/projects/lisp/cl-training/")

(defparameter *log-path* (merge-pathnames #p"data/training-cleaned.log" *path*))
(defparameter *org-report-path* (merge-pathnames #p"output/training-report.org" *path*))
(defparameter *images-path* (merge-pathnames #p"output/images/" *path*))
