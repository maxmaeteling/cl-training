(defpackage cl-training.parsers
    (:use :cl :maxpc :maxpc.digit :maxpc.char :parse-number))
(in-package :cl-training.parsers)

(defun =date ()
  (=destructure (y _ m _ d)
				(=list (=integer-number 10)
					   (?eq #\/)
					   (=integer-number 10)
					   (?eq #\/)
					   (=integer-number 10))
	(list y m d)))

(defun =float ()
  (=transform
   (=subseq
	(?seq (%some (?digit 10))
		  (%maybe (?seq (?eq #\.)
						(%any (?digit 10))))))
   #'parse-number:parse-number))

(defun =set ()
  (=destructure (sets _ reps _ weight) 
				(=list (=integer-number 10)
					   (?eq #\*)
					   (=integer-number 10)
					   (?eq #\*)
					   (=float))
	(list sets reps weight)))

(defun =exercise-name ()
  (=subseq (%some (%or (?satisfies 'alphanumericp)
					   (?satisfies (lambda (c)
									 (member c '(#\- #\_))))))))

(defun =exercise ()
  (=destructure (exercise _ reps)
				(=list (=exercise-name)
					   (%some (?whitespace))
					   (%some (=destructure (s _)
											(=list (=set)
												   (%maybe (?eq #\space)))
								s)))
	(list exercise reps)))
