(defpackage cl-training.helpers
  (:use :cl :local-time)
  (:export
   #:collate
   #:merge-adjoin
   #:hash-table-to-list
   #:listify
   #:timestamp-whole-week-difference
   #:timestamp-short-date
   #:transpose))

(in-package :cl-training.helpers)

(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun merge-adjoin (l1 l2 &key (test #'eql))
  (dolist (elem l1)
	(setf l2 (adjoin elem l2 :test test)))
  l2)

(defun collate (list &key (key #'car) (value #'identity) (test #'eql)
                       (merger (merge-adjoin :test #'eql)) (default nil))
  "https://funcall.blogspot.com/2025/03/collate-index-list.html"
  (let ((table (make-hash-table :test test)))
    (dolist (element list table)
      (let ((key (funcall key element)))
        (setf (gethash key table)
              (funcall merger (gethash key table default) (funcall value element)))))))

(defun listify (x)
  (if (consp x)
	  (list x)
	  x))

(defun hash-table-to-list (hash)
  (loop for k being the hash-keys of hash using (hash-value v) collect (list k v)))

(defun hash-table-rows (hash)
  (loop for k being the hash-keys of hash using (hash-value v) collect (cons k (listify v))))

(defun timestamp-whole-week-difference (time-a time-b)
  (truncate (/ (- (timestamp-to-unix time-a)
				  (timestamp-to-unix time-b))
			   (* 7 24 60 60))))

(defun timestamp-short-date (stream ts)
  (format-timestring stream
					 ts
					 :format '(:year "/" :month "/" :day)))
