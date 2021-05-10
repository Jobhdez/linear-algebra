(asdf:defsystem #:profiling
		:license "MIT License"
		:description "Profiling the system."
		:author "Job Hernandez"
		:version (:read-file-form "VERSION.txt")
		:depends-on (#:linear-algebra)
		:pathname "profiling/"
		:serial t
		:components ((:file "packages")
			     (:file "performance")))
