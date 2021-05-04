;;;; linear-algebra-tests.asd
;;;;
;;;; Author: Job Hernandez

(asdf:defsystem #:linear-algebra-tests
		:license "MIT License (see LICENSE.txt)"
		:description "Tests for Linear Algebra."
		:author "Job Hernandez"
		:version (:read-file-form "VERSION.txt")
		:depends-on (#:linear-algebra
			     #:fiasco)
		:perform (asdf:test-op (o s)
				       (uiop:symbol-call :linear-algebra-tests
							 '#:run-linear-algebra-tests))
		:pathname "tests/"
		:serial t
		:components ((:file "packages")
			     (:file "suite")
			     (:file "matrix-tests")))
