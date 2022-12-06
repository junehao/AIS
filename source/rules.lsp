;;		RULES.LSP														;
;;		By June-Hao Hou, March 2006										;

;;//////////////////////////////////////////////////////////////////////;
;;		Functions for ROB Data											;
;;//////////////////////////////////////////////////////////////////////;

;; Examples of Binding
;;
;; Bind the StartPoint of obj1 to the midpoint of obj2:
;; (bind obj1 "StartPoint"
;;		(list 'ais:point-at-ppt obj2 0.5))
;;
;; Bind the elevation of obj1 to the elevation of obj2:
;; (bind obj1 "elevation"
;;		(list 'vla-get-elevation obj2)) or
;; (bind obj1 "elevation" obj2) --> this is a direct copy.

;;----------------------------------------------------------------------;
;;		Function:	AIS:2D-POINT										;
;;																		;
;; Returned value:	variant (array of 2D points on XY-plane in WCS)		;
;;----------------------------------------------------------------------;
(defun ais:2d-point	(point-array)
  (LtoV	(apply 'append
			   (mapcar '(lambda (a) (left 2 a))
					   (chunk (VtoL point-array) 3)
			   )
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:BASEPOINT										;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:basepoint (obj) (vlax-3d-point (basepoint obj)))

;;----------------------------------------------------------------------;
;;		Function:	AIS:CENTER											;
;;																		;
;; Returned value:	variant (3D point)									;
;;----------------------------------------------------------------------;
(defun ais:center (obj)
  (cond	((spiral-p obj)
		 (vlax-3d-point (get-property obj :prop-Center))
		)
		((or (arc-p obj) (circle-p obj) (ellipse-p obj))
		 (vla-get-center obj)
		)
		((or (line-p obj)
			 (pline-p obj)
			 (pline3d-p obj)
			 (spline-p obj)
		 )
		 (ais:point-on-curve-at-ppt obj 0.5)
		)
		((point-p obj)
		 (vlax-3d-point
		   (vector-add
			 (vtol (vla-get-coordinates obj))
			 (mapcar '(lambda (a) (* (/ (vla-get-thickness obj) 2.0) a))
					 (vtol (vla-get-normal obj))
			 )
		   )
		 )
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:COLLECT-POINTS									;
;;																		;
;;	Description:	!!!Experimental!!!									;
;;					Collect points from a list of objects by the given	;
;;					function, then return the point list as a variant.	;
;;					!!!Conflict with ROG-Interpolate??					;
;;																		;
;;		Usage: (bind obj-A "coordinates"								;
;;					(list 'ais:collect-points 'ais:endpoint				;
;;						(list obj-B obj-C obj-D)) t)					;
;;----------------------------------------------------------------------;
(defun ais:collect-points (func lst)
  (LtoV (mapcar '(lambda (obj) (VtoL (apply func obj))) lst))
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:DIRECTION										;
;;----------------------------------------------------------------------;
(defun ais:direction (obj)
  (cond	((line-p obj)
		 (LtoV (mapcar '(lambda (u) (/ u (vla-get-length obj)))
				 (VtoL (ais:vector-sub (ais:endpoint obj) (ais:startpoint obj)))))
		)
		((or (point-p obj) (circle-p obj) (arc-p obj) (pline-p obj))
		 (vla-get-normal obj)
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:DIRECTION-BY-2POINTS							;
;;----------------------------------------------------------------------;
(defun ais:direction-by-2points	(point1 point2)
  (ais:vector-sub point2 point1)
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:ELEVATION										;
;;----------------------------------------------------------------------;
(defun ais:elevation (obj)
  (cond ((pline-p obj) (vla-get-elevation obj))
		(t (last (basepoint obj)))))

;;----------------------------------------------------------------------;
;;		Function:	AIS:ENDPOINT										;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:endpoint	(obj)
  (cond	((path-p obj) (vlax-3d-point (vlax-curve-getEndPoint obj)))
		((point-p obj)
		 (vlax-3d-point
		   (vector-add
			 (vtol (vla-get-coordinates obj))
			 (mapcar '(lambda (a) (* (vla-get-thickness obj) a))
					 (vtol (vla-get-normal obj))
			 )
		   )
		 )
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:GET-X											;
;;																		;
;; Returned value:	a float number.										;
;;----------------------------------------------------------------------;
(defun ais:get-x (point) (car (VtoL point)))

;;----------------------------------------------------------------------;
;;		Function:	AIS:GET-XY											;
;;																		;
;; Returned value:	a variant (2D point).								;
;;----------------------------------------------------------------------;
(defun ais:get-xy (point) (LtoV (left 2 (VtoL point))))

;;----------------------------------------------------------------------;
;;		Function:	AIS:GEt-Y											;
;;																		;
;; Returned value:	a float number.										;
;;----------------------------------------------------------------------;
(defun ais:get-y (point) (cadr (VtoL point)))

;;----------------------------------------------------------------------;
;;		Function:	AIS:GET-Z											;
;;																		;
;; Returned value:	a float number.										;
;;----------------------------------------------------------------------;
(defun ais:get-z (point) (caddr (VtoL point)))

;;----------------------------------------------------------------------;
;;		Function:	AIS:INTERPOLATE										;
;;----------------------------------------------------------------------;
(defun ais:interpolate (var1 var2 param)
  (LtoV (interpolate var1 var2 param))
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:INTERSECTWITH									;
;;----------------------------------------------------------------------;
(defun ais:intersectwith (obj1 obj2)
  (vla-intersectwith obj1 obj2 acExtendBoth))

(defun ais:intersectwithproj (curve pt norm)
  (vlax-curve-getclosestpointtoprojection curve pt norm))

;;----------------------------------------------------------------------;
;;		Function:	AIS:LENGTH											;
;;----------------------------------------------------------------------;
(defun ais:length (obj)
  (if (path-p obj)
	(- (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))
	   (vlax-curve-getDistAtParam
		 obj
		 (vlax-curve-getStartParam obj)
	   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:MIDPOINT										;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:midpoint	(obj)
  (ais:point-on-curve-at-ppt obj 0.5)
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:NUMBER-OF-POINTS								;
;;----------------------------------------------------------------------;
(defun ais:number-of-points	(obj)
  (cond	((pline-p obj)
		 (/	(length (VtoL (vla-get-coordinates obj)))
			2
		 )
		)
		((pline3d-p obj)
		 (/	(length (VtoL (vla-get-coordinates obj)))
			3
		 )
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:POINT-BETWEEN-2POINTS							;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:point-between-2points (pt1 pt2 param)
  (if (and (point-p pt1) (point-p pt2))
	(setq pt1 (ais:startpoint pt1)
		  pt2 (ais:startpoint pt2)
	)
  )
  (vlax-3d-point (interpolate pt1 pt2 param))
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:POINT-ON-CURVE-AT-DIST							;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:point-on-curve-at-dist (obj dist)
  (if (path-p obj)
	(vlax-3d-point (vlax-curve-getPointAtDist obj dist))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:POINT-ON-CURVE-AT-PARAM							;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:point-on-curve-at-param (obj param)
  (if (path-p obj)
	(vlax-3d-point (vlax-curve-GetPointAtParam obj param))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:POINT-ON-CURVE-AT-PPT							;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:point-on-curve-at-ppt (obj ppt)
  (if (path-p obj)
	(vlax-3d-point (curve-get-point-at-ppt obj ppt))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:STARTPOINT										;
;;																		;
;; Returned value:	a variant (3d point).								;
;;----------------------------------------------------------------------;
(defun ais:startpoint (obj)
  (cond	((path-p obj)
		 (vlax-3d-point (vlax-curve-getStartPoint obj))
		)
		((point-p obj) (vla-get-coordinates obj))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:TANGENT-ON-CURVE-AT-DIST						;
;;----------------------------------------------------------------------;
(defun ais:tangent-on-curve-at-dist	(obj dist)
  (ltov	(vlax-curve-getfirstderiv
		  obj
		  (vlax-curve-getparamatdist obj dist)
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:TANGENT-ON-CURVE-AT-PARAM						;
;;----------------------------------------------------------------------;
(defun ais:tangent-on-curve-at-param (obj param)
  (ltov	(vlax-curve-getfirstderiv
		  obj
		  (vlax-curve-getparamatpoint obj param)
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:TANGENT-ON-CURVE-AT-PPT							;
;;----------------------------------------------------------------------;
(defun ais:tangent-on-curve-at-ppt (obj ppt)
  (ltov	(vlax-curve-getfirstderiv
		  obj
		  (vlax-curve-getparamatpoint
			obj
			(curve-get-point-at-ppt obj ppt)
		  )
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:VECTOR-ADD										;
;;----------------------------------------------------------------------;
(defun ais:vector-add (point1 point2)
  (vlax-3d-point (mapcar '+ (VtoL point1) (VtoL point2)))
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:VECTOR-SUB										;
;;----------------------------------------------------------------------;
(defun ais:vector-sub (point1 point2)
  (vlax-3d-point (mapcar '- (VtoL point1) (VtoL point2)))
)


;;//////////////////////////////////////////////////////////////////////;
;;		UNUSED		UNUSED		UNUSED		UNUSED		UNUSED			;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	AIS:ALL-POINTS-ON-CURVE								;
;;																		;
;; Returned value:	variant (array of 3D points in WCS)					;
;;----------------------------------------------------------------------;
(defun ais:all-points-on-curve (obj)
  (cond
	;; For PLINE, combine 2D coordinates and elevation into 3D coordinates.
	((pline-p obj)
	 (setq elev (vla-get-elevation obj))
	 (vlax-list->variant
	   (apply 'append
			  (mapcar '(lambda (p) (append p (list elev)))
					  (chunk (vlax-variant->list (vla-get-coordinates obj)) 2)
			  )
	   )
	 )
	)
	((pline3d-p obj) (vla-get-coordinates obj))
	((spline-p obj) (vla-get-fitpoints obj))
  )
)


;; EOF