(defpackage #:profiling
  (:use #:linear-algebra
	#:sb-profile
	#:common-lisp)
  (:export #:programs
	   #:evaluate-programs))
