;;;; tests/packages.lisp
;;;;
;;;; Author: Job Hernandez

(fiasco:define-test-package #:linear-algebra-tests
			    (:use #:linear-algebra)
			    (:export
			     #:run-linear-algebra-tests))
