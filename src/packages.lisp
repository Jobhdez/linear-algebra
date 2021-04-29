(defpackage #:linear-algebra
  (:use #:common-lisp)
  (:export #:determinant
	   #:minor
	   #:cofactor
	   #:compute-trace
	   #:tref
	   #:upper-triangular
	   #:lower-triangular
	   #:transpose
	   #:inverse
	   #:make-matrix-of-minors
	   #:make-matrice!
	   #:get-matrix
	   #:multiply-matrices
	   #:add-matrices
	   #:multiply-by-scalar))
