;;; Author: Job Hernandez
;;; linear-algebra.lisp

(defpackage #:linear-algebra
  (:use #:common-lisp)
  (:export #:minor))

(in-package #:linear-algebra)


;; the Determinant whose elements are aij is denoted
;; |aij|

;; the Minor Mij is the remaining Determinant obtained
;; when removing the column and row containg the element
;; aij

;; the Cofactor of aij is the Signed Minor (-1)^i+j*Mij

(defun determinant (matrix row column)
  "Computes the determinate of a given matrix using 
  elements of the given column."
  ;; Matrix -> Number
  ;; given: '((1 -5 2) (7 3 4) (2 1 5))
  ;; expect:  148
  (let ((c (getcolumn column)))
    (if (null c)
	'()
      (+ (cofactor (minor matrix row column)
		   row
		   column)
	 (determinant matrix (+ row 1) column)))))

(defun minor (matrix row column)
  "Computes the minor of a matrix."
  ;; Matrix Row Column -> Matrix
  ;; given: '((2 3 4) (5 6 7) (8 9 10)) 2 1
  ;; expect: '((3 4) (9 10))
  (defun minor! (matrix col)
    "Takes a matrix with all rows except one, specified by 'row',
    and finishes computing the minor."
    (if (null matrix)
	'()
      (cons (removecol (car matrix) col)
	    (minor! (cdr matrix)
		    col))))
  (minor! (removerow matrix row)
	  column))

(defun removerow (matrix row)
  (removerowiter matrix row 1))

(defun removerowiter (matrix row counter)
  (if (= counter row)
      (cdr matrix)
    (cons (car matrix)
	  (removerowiter (cdr matrix)
			 row
			 (+ 1 counter)))))

(defun removecol (vec col)
  (removecoliter vec col 1))

(defun removecoliter (vec col counter)
  (if (= counter col)
      (cdr vec)
    (cons (car vec)
	  (removecoliter (cdr vec)
			 col
			 (+ counter 1)))))
		
  
(defun cofactor (minor row column)
  "Computes the cofactor of aij."
  (* (expt -1 (+ row column))
     (determinantn2 minor)))
		   
		   
		   
(fiasco:define-test-package #:minor-tests
			    (:use #:linear-algebra))
(in-package #:minor-tests)
(deftest test-compute-minor ()
  (is (equal (minor '((2 3 4) (5 6 7) (8 9 10)) 2 1)
	     '((3 4) (9 10)))))
      
