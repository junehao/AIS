;;		CURVE.LSP														;
;;		By June-Hao Hou, March 2006										;

;;----------------------------------------------------------------------;
;;		Function:	CURVE-GET-POINTS-AT-PPTS							;
;;																		;
;;		Description: This function returns a point list by the given	;
;;					proportion vlaues.									;
;;																		;
;; Returned value:	a point list in WCS.								;
;;----------------------------------------------------------------------;
(defun curve-get-points-at-ppts (obj proportions)
  (mapcar '(lambda (p) (curve-get-point-at-ppt obj p)) proportions))

;;----------------------------------------------------------------------;
;;		Function:	CURVE-GET-POINT-AT-PPT								;
;;																		;
;;		Description: This function returns the point at a proportion	;
;;					(0..1) on a curve.									;
;;																		;
;; Returned value:	a point in WCS.										;
;;----------------------------------------------------------------------;
(defun curve-get-point-at-ppt (obj proportion)
  (vlax-curve-getpointatdist
	obj
	(+ (* (- 1 proportion)
		  (vlax-curve-getdistatparam
			obj
			(vlax-curve-getstartparam obj)
		  )
	   )
	   (* proportion
		  (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj))
	   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PICK-PROPORTION-ON-CURVE							;
;;																		;
;;		Requirements:													;
;;					pick-N-ppt-on-curve									;
;;----------------------------------------------------------------------;
(defun pick-proportion-on-curve	(obj)
  (pick-N-value-on-curve obj 1 0)
)

;;----------------------------------------------------------------------;
;;		Function:	PICK-N-VALUE-ON-CURVE								;
;;																		;
;;	 Description:	This function asks the user to click N points		;
;;					on a curve then return their proportions in a list.	;
;;		Arguments:														;
;;			object	obj		a VLA object.								;
;;			int		N		Number of values to pick.					;
;;			int		vtype	Value type.									;
;;							0 = proportion (0..1)						;
;;							1 = paramter								;
;;							2 = fit point								;
;;							3 = distance								;
;;																		;
;;		Requirements:													;
;;					curve-get-ppt-at-point								;
;;																		;
;; Returned value:	a list of proportions.								;
;;----------------------------------------------------------------------;
(defun pick-N-value-on-curve (obj N vtype / screenPt curvePt ptObj value)
  (if (not save-osmode)
	(setq save-osmode (getvar "OSMODE"))
  )
  (if (not save-autosnap)
	(setq save-autosnap (getvar "AUTOSNAP"))
  )
  ;;(if (not save-pdmode)
  ;;	(setq save-pdmode (getvar "PDMODE"))
  ;;)
  (setvar "OSMODE" 512)		  ; 512 = Nearest point
  (setvar "AUTOSNAP" 1)		  ; 1 = Turn snap marker on
  ;;(setvar "PDMODE" 33)		  ; Set point display mode to 33 (= o)
  (command "UCS" "W")		  ; Make sure working in WCS.
  (if (not (and N (numberp N)))
	(setq N 1)
  )
  (while (> N 0)
	(terpri)
	(initget "eXit")
	(setq screenPt (getpoint "Select point on curve [eXit]: "))
	(cond
	  ((eq screenPt "eXit") (setq value nil) (setq N 0))
	  ((and	(vl-consp screenPt)
			(setq curvePt (vlax-curve-getclosestpointto obj screenPt))
	   )
	   ;; Add a point object as mark
	   (setq ptObj (cons ;;(vla-addPoint (model-space) (vlax-3d-point curvePt))
						 (crosshair3d-new curvePt)
						 ptObj
				   )
	   )
	   (cond ;; 0 = Proportion
			 ((= vtype 0)
			  (setq value (cons (curve-get-ppt-at-point obj curvePt) value))
			 )
			 ;; 1 = Paramter
			 ((= vtype 1)
			  (setq value (cons (vlax-curve-getParamAtPoint obj curvePt) value))
			 )
			 ;; 2 = fit point
			 ((= vtype 2)
			  (setq
				value (cons	(round (- (vlax-curve-getParamAtPoint obj curvePt)
									  (vlax-curve-getStartParam obj)
								   )
							)
							value
					  )
			  )
			 )
			 ;; 3 = Distance
			 ((= vtype 3)
			  (setq value (cons (vlax-curve-getDistAtPoint obj curvePt) value))
			 )
	   )
	   (setq N (1- N))
	  )
	)
  )
  ;; Restore system variabes
  (setvar "OSMODE" save-osmode)
  (setvar "AUTOSNAP" save-autosnap)
  ;;(setvar "PDMODE" save-pdmode)
  (setq	save-osmode	nil
		save-autosnap
		 nil
		 ;;		save-pdmode	nil
  )
  ;; Remove tentative point objects and regen viewport
  (mapcar 'vla-delete ptObj)
  ;;(vla-regen (active-document) acActiveViewport)
  (reverse value)
)

;;----------------------------------------------------------------------;
;;		Function:	CURVE-GET-PPT-AT-POINT								;
;;																		;
;;		Description: This function returns the proportion (0..1) of a	;
;;					point on the curve.									;
;;																		;
;; Returned value:	a float number (0..1).								;
;;----------------------------------------------------------------------;
(defun curve-get-ppt-at-point (obj pt)
  (/ (-	(vlax-curve-getdistatpoint obj pt)
		(vlax-curve-getdistatpoint obj (vlax-curve-getstartpoint obj))
	 )
	 (-	(vlax-curve-getdistatpoint obj (vlax-curve-getendpoint obj))
		(vlax-curve-getdistatpoint obj (vlax-curve-getstartpoint obj))
	 )
  )
)

;;----------------------------------------------------------------------;
;;		 Function:  UCS-ON-CURVE-AT-PPT									;
;;																		;
;;	  Description:  This function sets the UCS at a proportion			;
;;					on the curve.										;
;;																		;
;;					X = toward spiral center axis						;
;;					Y = points up										;
;;					Z = toward the tangent of spiral curve				;
;;----------------------------------------------------------------------;
(defun ucs-on-curve-at-ppt (curve ppt / point1 point2)
  (command "UCS" "W")
  (setq	point2 (mapcar '+
					   (setq point1 (vlax-curve-getpointatparam curve ppt))
					   (vlax-curve-getfirstderiv curve ppt)
			   )
  )
  (command "UCS" "N" "ZA" point1 point2)
)
  
;;----------------------------------------------------------------------;
;;		Function:  UCS-THRU-CURVE										;
;;----------------------------------------------------------------------;
(defun ucs-thru-curve (curve / startp endp step)
  (command-begin)
  (command "_UCSICON" "_OFF")
  (setq startp (vlax-curve-getstartparam curve))
  (setq endp (vlax-curve-getendparam curve))
  (setq step (/ (- endp startp) 36.0))
  (while (<= startp endp)
	(ucs-on-curve-at-ppt curve startp)
	(command "LINE" '(0 0 0) '(0 40 0) "")
	(command "LINE" '(0 0 0) '(80 0 0) "")
	(setq startp (+ startp step))
  )
  (command "UCS" "W")
  (command "_UCSICON" "_ON")
  (command-end)
)

;; EOF