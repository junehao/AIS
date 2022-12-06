;;		MATH.LSP														;
;;		By June-Hao Hou, March 2006										;

;;----------------------------------------------------------------------;
;;		Function:	CYLINDRICAL-POINT									;
;;																		;
;;	 Description:	Returns a WCS point on cyclindrical coordinate		;
;;					(radius, theta, z).									;
;;																		;
;; Returned value:	a point list.										;
;;----------------------------------------------------------------------;
(defun cylindrical-point (radius theta z)
  (list (* radius (cos theta)) (* radius (sin theta)) z)
)

;;----------------------------------------------------------------------;
;;		Function:	INTERPOLATE											;
;;																		;
;;	 Description:	This recursive function calculates intepolation 	;
;;					of two incoming source values. They can be numbers,	;
;;					list of numbers, or variants (VLA array of number).	;
;;																		;
;;		Arguments:														;
;;			mix		val1		Number, list, or variant.				;
;;			mix		val2		Number, list, or variant.				;
;;			float	param		Proportion, 0..1.						;
;;																		;
;; Returned value:	mixed, depends on the input values.					;
;;----------------------------------------------------------------------;
(defun interpolate (val1 val2 param)
  (cond	;; if both val1 and val2 are numbers, do calculation.
		((and (numberp val1) (numberp val2))
		 (+ (* param val2) (* (- 1 param) val1))
		)
		;; If both val1 and val2 are variants, convert them into list and resend.
		((and (variant-p val1) (variant-p val2))
		 (interpolate
		   (VTOL val1)
		   (VTOL val2)
		   param
		 )
		)
		;; If both val1 and val2 are lists, mapcar them.
		((and (vl-consp val1) (vl-consp val2))
		 (mapcar '(lambda (a b) (interpolate a b param)) val1 val2)
		)
		(t nil)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CEILING												;
;;----------------------------------------------------------------------;
(defun ceiling (num)
  (if (= num (fix num))
	(fix num)
	(1+ (fix num))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	FLOOR												;
;;----------------------------------------------------------------------;
(defun floor (num) (fix num))

;;----------------------------------------------------------------------;
;;		Function:	ROUND												;
;;----------------------------------------------------------------------;
(defun round (num)
  (if (minusp num)
	(fix (- num 0.5))
	(fix (+ num 0.5))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SERIES												;
;;																		;
;;		Usage:		(series 1 10 2) => (1 3 5 7 9)						;
;;----------------------------------------------------------------------;
(defun series (start end inc)
  (cond	((= start end) (list end))
		((and (> inc 0) (> start end)) nil)
		((and (< inc 0) (< start end)) nil)
		(T (cons start (series (+ start inc) end inc)))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SERIES2												;
;;																		;
;;	Description:	Options to skip start and/or end items. 			;
;;																		;
;;		Usage:		(series2 1 9 2 t t)     =>	(1 3 5 7 9)				;
;;					(series2 1 9 2 nil t)   =>	  (3 5 7 9)				;
;;					(series2 1 9 2 t nil)   =>	(1 3 5 7)				;
;;					(series2 1 9 2 nil nil) =>	  (3 5 7)				;
;;----------------------------------------------------------------------;
(defun series2 (start end inc start? end?)
  (series (if start?
			start
			(+ start inc)
		  )
		  (if end?
			end
			(- end inc)
		  )
		  inc
  )
)

;;----------------------------------------------------------------------;
;;		Function:	BULGE												;
;;----------------------------------------------------------------------;
(defun bulge (rad) (/ (sin (/ rad 4.0)) (cos (/ rad 4.0))))

;;----------------------------------------------------------------------;
;;		Function:	RADIAN												;
;;----------------------------------------------------------------------;
(defun radian (ang) (* pi (/ ang 180.0)))

;; EOF