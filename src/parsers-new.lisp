(defpackage cl-training.parsers-new
  (:use :cl :maxpc :maxpc.digit :maxpc.char :parse-number :local-time)
  (:export :=trainings))
(in-package :cl-training.parsers-new)

(defun =int ()
  (=integer-number 10))

(defun =date ()
  (=destructure (y _ m _ d)
				(=list (=int) (?eq #\/) (=int) (?eq #\/) (=int))
	(encode-timestamp 0 0 0 0 d m y)))

(defun =word ()
  (=subseq (%some (%or (?satisfies 'alpha-char-p)
					   (?satisfies (lambda (c)
									 (member c '(#\- #\_ #\( #\)))))))))

(defun =float ()
  (=transform
   (=subseq
	(?seq (%some (?digit 10))
		  (%maybe (?seq (?eq #\.)
						(%any (?digit 10))))))
   #'parse-number:parse-number))

(defun =separated (fn-x fn-sep)
  (%some (=transform (=list (funcall fn-x)
							(%any (funcall fn-sep)))
					 #'first)))
(defun ?space ()
  (?eq #\ ))

(defun =comma-separated (fn)
  (=separated fn #'(lambda () (?eq #\,))))

(defun =comma-separated-ints ()
  (=comma-separated #'=int))

(defun =comma-separated-floats ()
  (=comma-separated #'=float))

(defun =rest-of-line ()
  (%any (%diff (?newline))))

(defun =set-expr ()
  (=transform
   (=list
	(=separated #'=comma-separated-floats
				#'(lambda () (?eq #\*)))
	(%maybe (?seq (?eq #\+)
				  (=rest-of-line))))
   #'first))

(defun =exercise-name ()
  (=subseq (?seq (=word)
				 (%any (?seq (%maybe (%some (?whitespace)))
							 (=word))))))

(defun =exercise ()
  (=destructure (exercise _ sets)
				(=list (=exercise-name)
					   (%some (?whitespace))
					   (=separated #'=set-expr #'?space))
	(list exercise sets)))

(defun =training ()
  (=destructure (d _ e)
				(=list (=date)
					   (?newline)
					   (=separated #'=exercise #'?newline))
	(list d e)))

(defun =trainings ()
  (=list (=separated #'=training #'?newline)))