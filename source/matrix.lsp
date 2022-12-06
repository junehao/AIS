;;		MATRIX.LSP														;
;;		By June-Hao Hou, March 2006										;

;;//////////////////////////////////////////////////////////////////////;
;; PREDICATE FUNCTIONS													;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	MATRIX-P											;
;;																		;
;;	Description:	Determine if the given list is a well-formed matrix.;
;;----------------------------------------------------------------------;
(defun matrix-p	(lst)
  (if (apply 'and (mapcar 'vl-consp lst))
	(apply 'and
		   (mapcar '(lambda (a) (= (length (car lst)) (length a))) lst)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ROW-VECTOR-P										;
;;																		;
;;	Description:	Determine if the given list is a row vector.		;
;;----------------------------------------------------------------------;
(defun row-vector-p	(lst)
  (and (matrix-p lst) (= (num-rows lst) 1))
)

;;----------------------------------------------------------------------;
;;		Function:	COL-VECTOR-P										;
;;																		;
;;	Description:	Determine if a list is a column vector.				;
;;----------------------------------------------------------------------;
(defun col-vector-p	(lst)
  (and (matrix-p lst)
	   (= (num-rows lst) (length (apply 'append lst)))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	VECTOR-P											;
;;																		;
;;	Description:	Determine if a list is a vector.					;
;;----------------------------------------------------------------------;
(defun vector-p	(lst)
  (or (row-vector-p lst) (col-vector-p lst))
)

;;----------------------------------------------------------------------;
;;		Function:	SQUARE-MATRIX-P										;
;;																		;
;;	Description:	Determine if a list is a square matrix.				;
;;----------------------------------------------------------------------;
(defun square-matrix-p (lst)
  (and (matrix-p lst) (= (num-rows lst) (num-cols lst)))
)


;;//////////////////////////////////////////////////////////////////////;
;;		MATRIX OPERATION												;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	MATRIX-DIMENSION									;
;;																		;
;;	Description:	Get dimension of a matrix.							;
;;----------------------------------------------------------------------;
(defun matrix-dimension	(m)
  (cons (num-rows m) (num-cols m))
)

;;----------------------------------------------------------------------;
;;		Function:	NUM-ROWS											;
;;																		;
;;	Description:	Get number of rows of a matrix.						;
;;----------------------------------------------------------------------;
(defun num-rows (m) (length m))

;;----------------------------------------------------------------------;
;;		Function:	NUM-COLS											;
;;																		;
;;	Description:	Get number of columns of a matrix.					;
;;----------------------------------------------------------------------;
(defun num-cols (m) (length (car m)))

;;----------------------------------------------------------------------;
;;		Function:	TRANSPOSE*											;
;;																		;
;;	Description:	Returns the transposition of a matrix (with type 	;
;;					checking).											;
;;----------------------------------------------------------------------;
(defun transpose* (m)
  (if (matrix-p m)
	(transpose m)
	(*error* "Not a matrix!")
  )
)

;;----------------------------------------------------------------------;
;;		Function:	TRANSPOSE											;
;;																		;
;;	Description:	Returns the transposition of a matrix.				;
;;----------------------------------------------------------------------;
(defun transpose (m)
  (if (= (length (car m)) 1)
	(list (mapcar 'car m))
	(cons (mapcar 'car m) (transpose (mapcar 'cdr m)))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MATRIX-ADD											;
;;----------------------------------------------------------------------;
(defun matrix-add (A B)
  (if (and (matrix-p A)
		   (matrix-p B)
		   (= (num-rows A) (num-rows B))
		   (= (num-cols A) (num-cols B))
	  )
	(mapcar '(lambda (A-row B-row) (mapcar '+ A-row B-row)) A B)
	(*error* "Matrices mismatch!")
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MATRIX-SUBTRACT										;
;;----------------------------------------------------------------------;
(defun matrix-subtract (A B)
  (if (and (matrix-p A)
		   (matrix-p B)
		   (= (num-rows A) (num-rows B))
		   (= (num-cols A) (num-cols B))
	  )
	(mapcar '(lambda (A-row B-row) (mapcar '- A-row B-row)) A B)
	(*error* "Matrices mismatch!")
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MATRIX-PRODUCT										;
;;																		;
;;	Description:	The wrapper function of cross product of matrices.	;
;;----------------------------------------------------------------------;
(defun matrix-product (A B)
  (if (matrix-p B)
	(cond ((numberp A)		  ; In case of scalar product...
		   (matrix-scalar-product A B)
		  )
		  ((matrix-p A)		  ; In case of matrix product...
		   (if (= (num-cols A) (num-rows B))
			 (matrix-product* A B)
			 (*error* "Not matrices or matrices mismatch!")
		   )
		  )
	)
	(*error* "The 2nd parameter must be a matrix!")
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MATRIX-SCALAR-PRODUCT								;
;;																		;
;;	Description:	The real function calculating the scalar product.	;
;;----------------------------------------------------------------------;
(defun matrix-scalar-product (S B)
  (mapcar '(lambda (B-row) (mapcar '(lambda (cell) (* S cell)) B-row))
		  B
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MATRIX-PRODUCT*										;
;;																		;
;;	Description:	The real function calculating the matrix product.	;
;;----------------------------------------------------------------------;
(defun matrix-product* (A B / BT)
  (setq BT (transpose* B))
  (mapcar '(lambda (A-row)
			 (mapcar '(lambda (B-row) (apply '+ (mapcar '* A-row B-row)))
					 BT
			 )
		   )
		  A
  )
)

;;----------------------------------------------------------------------;
;;		Function:	VECTOR-ADD											;
;;----------------------------------------------------------------------;
(defun vector-add (A B) (mapcar '+ A B))

;;----------------------------------------------------------------------;
;;		Function:	VECTOR-SUBTRACT										;
;;----------------------------------------------------------------------;
(defun vector-subtract (A B) (mapcar '- A B))

;;----------------------------------------------------------------------;
;;		Function:	VECTORIZE											;
;;																		;
;;		Usage:		(vectorize '((x1 y1 z1) (x2 y2 z2) ...)				;
;;					=> ((0 0 0) (x2-x1 y2-y1 z2-z1) ...) 				;
;;----------------------------------------------------------------------;
(defun vectorize (lst / origin)
  (if (vl-consp (setq origin (car lst)))
	(cons (vector-subtract origin origin)
		  (mapcar '(lambda (pt) (vector-subtract pt origin))
				  (cdr lst)
		  )
	)
  )
)

;;//////////////////////////////////////////////////////////////////////;
;; TRANSFORMATION MATRIX FUNCTIONS										;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	TMATRIX-TRANS										;
;;																		;
;;	Description:	Generate a translation matrix.						;
;;----------------------------------------------------------------------;
(defun tmatrix-trans (x y z)
  (list	(list 1 0 0 x)
		(list 0 1 0 y)
		(list 0 0 1 z)
		(list 0 0 0 1)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	TMATRIX-SCALE										;
;;																		;
;;	Description:	Generate a scale matrix.							;
;;----------------------------------------------------------------------;
(defun tmatrix-scale (x y z)
  (list	(list x 0 0 0)
		(list 0 y 0 0)
		(list 0 0 z 0)
		(list 0 0 0 1)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	TMATRIX-ROT											;
;;																		;
;;	Description:	Generate a rotation matrix (a wrapper function).	;
;;----------------------------------------------------------------------;
(defun tmatrix-rot (axis ang)
  (cond	((eq axis 'x) (tmatrix-rot-x ang))
		((eq axis 'y) (tmatrix-rot-y ang))
		((eq axis 'z) (tmatrix-rot-z ang))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	TMATRIX-ROT-X										;
;;																		;
;;	Description:	Generating a rotation matrix by X-axis.				;
;;----------------------------------------------------------------------;
(defun tmatrix-rot-x (rho)
  (list	'(1 0 0 0)
		(list 0 (cos rho) (* -1 (sin rho)) 0)
		(list 0 (sin rho) (cos rho) 0)
		'(0 0 0 1)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	TMATRIX-ROT-Y										;
;;																		;
;;	Description:	Generating a rotation matrix by Y-axis.				;
;;----------------------------------------------------------------------;
(defun tmatrix-rot-y (beta)
  (list	(list (cos beta) 0 (sin beta) 0)
		'(0 1 0 0)
		(list (* -1 (sin beta)) 0 (cos beta) 0)
		'(0 0 0 1)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	TMATRIX-ROT-Z										;
;;																		;
;;	Description:	Generating a rotation matrix by Z-axis.				;
;;----------------------------------------------------------------------;
(defun tmatrix-rot-z (theta)
  (list	(list (cos theta) (* -1 (sin theta)) 0 0)
		(list (sin theta) (cos theta) 0 0)
		'(0 0 1 0)
		'(0 0 0 1)
  )
)

;; EOF