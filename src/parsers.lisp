(defpackage cl-training.parsers
  (:use :cl :maxpc :maxpc.digit :maxpc.char :parse-number :cl-training.classes)
  (:export :=trainings
		   :=exercise-alias-lists
		   :=exercise-alias-list
		   :=exercise-aliases
		   :=quoted-string))
(in-package :cl-training.parsers)

(defun =int ()
  (=integer-number 10))

(defun =date ()
  (=destructure (y _ m _ d)
				(=list (=int) (?eq #\/) (=int) (?eq #\/) (=int))
	(list y m d)))

(defun =float ()
  (=transform
   (=subseq
	(?seq (%some (?digit 10))
		  (%maybe (?seq (?eq #\.)
						(%any (?digit 10))))))
   #'parse-number:parse-number))

(defun =comma-separated (fn)
  (=destructure (f l)
				(=list (funcall fn)
					   (%any (=destructure (_ n) (=list (?eq #\,)
														(funcall fn))
							   n)))
	(cons f l)))

(defun =comma-separated-ints ()
  (=comma-separated #'=int))

(defun =comma-separated-floats ()
  (=comma-separated #'=float))

(defun =set-only-reps ()
  (=transform (=comma-separated-ints)
			  #'(lambda (reps) (make-exercise-set reps))))

(defun =set-reps-weights ()
  (=destructure (reps weights)
				(=list (=comma-separated-ints)
					   (=destructure (_ weights)
									 (=list (?eq #\*)
											(=comma-separated-floats))
						 weights))
	(normalize-reps-weights reps weights)))

(defun =set-sets-reps-weights ()
  (=destructure (sets rw)
				(=list (=transform (=list (=comma-separated-ints)
										  (?eq #\*))
								   #'first)
					   (=set-reps-weights))
	(multiply-sets sets rw)))

(defun normalize-reps-weights (reps weights)
  (mapcar #'(lambda (set)
			  (destructuring-bind (reps weight) set
				(make-set-weight reps weight)))
		   (cartesian-product reps weights)))

(defun cartesian-product (l1 l2)
  (loop
	for x in l1
	append (loop
			  for y in l2
			  collect (list x y))))

(defun =set ()
  (%or (=set-sets-reps-weights)
	   (=set-reps-weights)
	   (=set-only-reps)))

(defun =word ()
  (=subseq (%some (%or (?satisfies 'alpha-char-p)
					   (?satisfies (lambda (c)
									 (member c '(#\- #\_ #\( #\)))))))))

(defun =exercise-name ()
  (=subseq (?seq (=word)
				 (%any (?seq (%maybe (%some (?whitespace)))
							 (=word))))))

(defun =exercise ()
  (=destructure (exercise _ reps)
				(=list (=exercise-name)
					   (%some (?whitespace))
					   (%some (=transform (=list (=set)
												 (%maybe (?eq #\space)))
										  #'first)))
	(make-exercise exercise reps)))

(defun =training ()
  (=destructure (d _ e)
				(=list (=date)
					   (?newline)
					   (%some (=destructure (tr _)
											(=list (=exercise)
												   (%maybe (?newline)))
								tr)))
	(make-training d e)))

(defun =separated (fn-x fn-sep)
  (%some (=transform (=list (funcall fn-x)
							(%any (funcall fn-sep)))
					 #'first)))

(defun =trainings ()
  (=list (=separated #'=training #'?newline)))

(defun =exercise-alias-lists ()
  (=list (=separated #'=exercise-aliases #'?newline)))

(defun ?space ()
  (?eq #\ ))

(defun =exercise-aliases ()
  (=separated #'=quoted-string #'?space))

(defun ?quote ()
  (?eq #\"))

(defun =quoted-string ()
  (=transform (=list (?quote)
					 (=subseq (%any (?not (?quote))) )
					 (?quote))
			  #'cadr))