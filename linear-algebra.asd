(asdf:defsystem #:linear-algebra
		:description "Some Linear Algebra."
		:author "Job Hernandez <hj93@protonmail.com>"
		:version (:read-file-form "VERSION.txt")
		:license "MIT License (see LICENSE.txt)"
		:in-order-to ((asdf:test-op (asdf:test-op #:linear-algebra-tests)))
		:around-compile (lambda (compile)
				  (let (#+sbcl (sb-ext:*derive-function-types* t))
				    (funcall compile)))
		:serial t
		:pathname "src/"
		:components
		((:file "packages")
		 (:module "linear-algebra"
			  :serial t
			  :components ((:file "matrix")))))
