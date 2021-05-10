(in-package #:profiling)

(defparameter programs '((determinant '((1 -5 2) (7 3 4) (2 1 5)) 1)
		         (minor '((2 3 4) (5 6 7) (8 9 10)) 2 1)
		         (cofactor '((2 3 4) (5 6 7) (8 9 10)) 2 1)
		         (tref '((3 4) (4 5) (6 7)) 3 2)
		         (compute-trace '((2 3 4) (6 8 9) (1 10 12)))
		         (upper-triangular '((2 3 4) (5 6 7) (8 9 10)))
		         (lower-triangular '((2 3 4) (5 6 7) (8 9 10)))
		         (transpose '((2 3 4) (5 6 7)))
		         (inverse '((-1 -2 2) (2 1 1) (3 4 5)))
		         (get-matrix (add-matrices (make-matrice! 2 3 '((1 2 3) (4 5 6)))
					           (make-matrice! 2 3 '((1 2 3) (4 5 6)))))
		         (get-matrix (multiply-matrices (make-matrice! 2 3 '((1 2 3) (4 5 6)))
						        (make-matrice! 2 3 '((7 8) (9 10) (11 12)))))
		         (multiply-by-scalar 5 '((2 3 4) (5 6 7)))))

(defun evaluate-programs (progs)
  (loop for i in progs
	do (eval i)))		    
		   
(sb-profile:profile determinant
		    minor
		    cofactor
		    compute-trace
		    upper-triangular
		    lower-triangular
		    inverse
		    get-matrix
		    make-matrice!
		    add-matrices
		    multiply-matrices
		    multiply-by-scalar)

(evaluate-programs programs)

(sb-profile:report)
