(defpackage cl-training/tests/main
  (:use :cl
        :cl-training
        :rove))
(in-package :cl-training/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-training)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
