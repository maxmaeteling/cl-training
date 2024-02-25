(defsystem "cl-training"
  :version "0.1.0"
  :author "Max Mäteling"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-training/tests"))))

(defsystem "cl-training/tests"
  :author "Max Mäteling"
  :license ""
  :depends-on ("cl-training"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-training"
  :perform (test-op (op c) (symbol-call :rove :run c)))
