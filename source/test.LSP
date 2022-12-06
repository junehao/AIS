;;----------------------------------------------------------------------;
;;		Function:	C:SPIRAL											;
;;																		;
;;		Requirements:													;
;;					spiral-curve										;
;;----------------------------------------------------------------------;
(defun c:spiral	()
  (setq pt0 (getpoint "Center point: "))
  (setq pt1 (getpoint "Start point: "))
  (setq ang0 (angle pt0 pt1))
  (setq radius (distance pt0 pt1))
  (setq total-height (getreal "Total height: "))
  (setq total-angle (radian (getreal "Total angle (in degree): ")))
  (setq num-intervals (getint "Number of intervals: "))
  (initget 0 "Polyline Spline")
  (setq obj-type (getint "Type of object [Polyline/Spline] <P>: "))
  (cond	((or (eq obj-type "Polyline") (= obj-type 1))
		 (obj-type = 1)
		)
		((or (eq obj-type "Spline") (= obj-type 2)) (obj-type = 2))
		(t (obj-type = 1))
  )
  (spiral-curve
	pt0	ang0 radius	total-angle	total-height num-intervals obj-type)
)

;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-CURVE										;
;;																		;
;;	  	Description: This function creates a spiral curve as a			;
;;					3DPolyline or a Spline object.						;
;;																		;
;;		Arguments:														;
;;			list	ctr			A point in WCS.							;
;;			float	ang0		Starting angle in radian.				;
;;			float	rad			Radius.									;
;;			float	ang			Total angle in radian.					;
;;			float	height		Total height.							;
;;			int		num-int		Number of intervals.					;
;;			int		flag		1: 3dpolyline,							;
;;								2: Spline curve.						;
;;																		;
;;		Requirements:													;
;;					spiral-pointlist									;
;;					spiral-get-tangent-at-angle							;
;;																		;
;; Returned value:	a VLA object.										;
;;----------------------------------------------------------------------;
(defun spiral-curve	(ctr ang0 rad ang height num-int flag / point-list)
  (setq point-list (spiral-pointlist ctr ang0 rad ang height num-int))
  (cond	;; 1 -> Returns a 3DPolyline
		((= flag 1)
		 (vla-add3dpoly
		   (model-space)
		   (vlax-list->variant (apply 'append point-list))
		 )
		)
		;; 2 -> Returns a Spline curve
		((= flag 2)
		 (vla-addSpline
		   (model-space)
		   (vlax-list->variant (apply 'append point-list))
		   ;; Start tangent
		   (vlax-3d-point
			 (spiral-get-tangent-at-angle ang0 rad ang height)
		   )
		   ;; End tangent
		   (vlax-3d-point
			 (spiral-get-tangent-at-angle ang rad ang height)
		   )
		 )
		)
  )
)


;;----------------------------------------------------------------------;
;;		Function:  SPIRAL-POINTLIST										;
;;																		;
;;	  	Description: This function creates a spiral pointlist.			;
;;																		;
;;		Arguments:														;
;;			list	center			A point in WCS.						;
;;			float	angle0			Starting angle in radian.			;
;;			float	radius			Radius.								;
;;			float	total-angle		Total angle in radian.				;
;;			float	total-height	Total height.						;
;;			int		num-int			Number of intervals.				;
;;																		;
;;		Requirements:													;
;;					cylindrical-point									;
;;																		;
;; Returned value:	a point list in the form of: 						;
;;					((x1 y1 z1) (x2 y1 z2) ...)							;
;;----------------------------------------------------------------------;
(defun spiral-pointlist	(center		  angle0	   radius		total-angle
						 total-height num-int	   /			angle-inc
						 height-inc	  index		   point-list	pt
						)
  (setq angle-inc (/ total-angle num-int))
  (setq height-inc (/ total-height num-int))
  (setq index 0)
  (setq point-list nil)
  (while (<= index num-int)
	(setq pt (mapcar '+
					 center
					 (cylindrical-point
					   radius
					   (+ (* index angle-inc) start-angle)
					   (* index height-inc)
					 )
			 )
	)
	(setq point-list (append point-list (list pt)))
	(setq index (1+ index))
  )
  point-list
)

;;----------------------------------------------------------------------;
;;		 Function:  SPIRAL-GET-TANGENT-AT-ANGLE							;
;;																		;
;;	  Description:  This function returns the normal vector of a spiral	;
;;		 		    at a specific angle.								;
;;																		;
;;					Tx = -R * sin(theta)								;
;;					Ty =  R * cos(theta)								;
;;						where R: radius, theta: angle.					;
;;					Tz = R * H / (R * rho) = H / rho					;
;;						where H: total height, rho: total angle.		;
;;																		;
;; Returned value:	a point list in the form of: 						;
;;----------------------------------------------------------------------;
(defun spiral-get-tangent-at-angle (ang radius total-ang total-high)
  (list	(* radius (sin ang) -1)
		(* radius (cos ang))
		(/ (float total-high) total-ang)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CYLINDRICAL-POINT									;
;;																		;
;;		Description: Returns a WCS point on cyclindrical coordinate		;
;;					(radius, theta, z).									;
;;																		;
;;		Requirements:													;
;;				vla-PolarPoint (Utility object)							;
;;																		;
;; Returned value:	a point list.										;
;;----------------------------------------------------------------------;
(defun cylindrical-point (radius theta z)
  (vlax-variant->list
	(vla-PolarPoint
	  util
	  (vlax-3d-point (list 0 0 z))
	  theta
	  radius
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
  (pick-N-ppt-on-curve obj 1)
)

;;----------------------------------------------------------------------;
;;		Function:	PICK-N-PPT-ON-CURVE									;
;;																		;
;;		Description: This function asks the user to click N points		;
;;					on a curve then return their proportions in a list.	;
;;																		;
;;		Requirements:													;
;;					curve-get-ppt-at-point								;
;;																		;
;; Returned value:	a list of proportions.								;
;;----------------------------------------------------------------------;
(defun pick-N-ppt-on-curve (obj N / screenPt curvePt ptObj proportion)
  (if (not save-osmode)
	(setq save-osmode (getvar "OSMODE"))
  )
  (if (not save-autosnap)
	(setq save-autosnap (getvar "AUTOSNAP"))
  )
  (if (not save-pdmode)
	(setq save-pdmode (getvar "PDMODE"))
  )
  (setvar "OSMODE" 512)		  ; 512 = Nearest point
  (setvar "AUTOSNAP" 1)		  ; 1 = Turn snap marker on
  (setvar "PDMODE" 33)		  ; Set point display mode to 33 (= o)
  (command "UCS" "W")		  ; Make sure working in WCS.
  (if (not (and N (numberp N)))
	(setq N 1)
  )
  (while (> N 0)
	(terpri)
	(initget "eXit")
	(setq screenPt (getpoint "Select point on curve [eXit]: "))
	(cond ((eq screenPt "eXit") (setq proportion nil) (setq N 0))
		  ((and	(vl-consp screenPt)
				(setq curvePt (vlax-curve-getclosestpointto obj screenPt))
		   )
		   ;; Add a point object as mark
		   (setq
			 ptObj (cons (vla-addPoint (model-space) (vlax-3d-point curvePt))
						 ptObj
				   )
		   )
		   (setq proportion
				  (cons (curve-get-ppt-at-point obj curvePt) proportion)
		   )
		   (setq N (1- N))
		  )
	)
  )
  ;; Restore system variabes
  (setvar "OSMODE" save-osmode)
  (setvar "AUTOSNAP" save-autosnap)
  (setvar "PDMODE" save-pdmode)
  (setq	save-osmode	nil
		save-autosnap nil
		save-pdmode	nil
  )
  ;; Remove tentative point objects and regen viewport
  (mapcar 'vla-delete ptObj)
  (vla-regen (active-document) acActiveViewport)
  (reverse proportion)
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
  (/ (-	(vlax-curve-getparamatpoint obj pt)
		(vlax-curve-getstartparam obj)
	 )
	 (-	(vlax-curve-getendparam obj)
		(vlax-curve-getstartparam obj)
	 )
  )
)


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
  (vlax-curve-getPointAtParam
	obj
	(+ (vlax-curve-getStartParam obj)
	   (* (- (vlax-curve-getEndParam obj)
			 (vlax-curve-getStartParam obj)
		  )
		  proportion
	   )
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
;;		 Function:  UCS-THRU-CURVE										;
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

(defun test (obj1 obj2 steps)
  (setq ppts (series 0.0 1.0 (/ 1.0 steps)))
  (mapcar '(lambda (p1 p2) (command "LINE" p1 p2 ""))
		  (curve-get-points-at-ppts obj1 ppts)
		  (curve-get-points-at-ppts obj2 ppts)))

;|
(setq script1 '((setq
				 s1
				 (spiral-new '(0 0 0) (/ 2.98 2) 0 (/ pi 8) (/ 2.98 16) 72)
				)
				(setq
				 s2
				 (spiral-new '(0 0 0) (/ 8.86 2) 0 (/ pi 8) (/ 2.98 16) 72)
				)
				(mapcar
				 '(lambda (a b)
					(vla-add3dPoly
					  (model-space)
					  (vlax-list->variant (append a b))
					)
				  )
				 (every
				  2
				  (chunk (vlax-variant->list (vla-get-coordinates s1)) 3)
				 )
				 (every
				  2
				  (chunk (vlax-variant->list (vla-get-coordinates s2)) 3)
				 )
				)
				(setq
				 s3
				 (spiral-new
				  '(0 0 1.33)
				  (+ (/ 2.98 2) 0.5)
				  0
				  (/ pi 4)
				  (/ 2.98 8)
				  36
				 )
				)
				(vla-put-color s3 6) ; magenta
				(mapcar
				 '(lambda (a b)
					(vla-addCylinder
					  (model-space)
					  (vlax-list->variant a)
					  b
					  2.66
					)
				  )
				 (chunk (vlax-variant->list (vla-get-coordinates s3)) 3)
				 (series 0.25 0.135 (/ (- 0.25 0.135) -36))
				)
				;;(vla-put-visible s3 :vlax-false)
			   )
)
|;

(setq children '((a . 0.0) b c (d . 0.5) e f g h i j (k . 1.0)))
(defun calc-param (lst / index counter result)
  (setq result nil)
  (foreach item	lst
	(if	(vl-consp item)
	  (progn
		(if	(not (null result))
		  (progn (setq step (/ (- (cdr item) (last result)) (1+ counter)))
				 (repeat counter
				   (setq result (append result (list (+ (last result) step))))
				 )
		  )
		)
		(setq result (append result (list (cdr item))))
		(setq counter 0)
	  )
	  (setq counter (1+ counter))
	)
  )
  result
)


(defun cb-test-modified (notifier reactor data)
  (terpri)
  (princ notifier)
  (princ " - ")(princ reactor)
  (princ " - ")(princ data)
  )
(defun cb-test-copied (notifier reactor data)
  (if data (progn
  (terpri)(princ "Copied: ")
  (princ notifier)
  (princ " - ")(princ reactor)
  (princ " - ")(princ data)
  )))
									