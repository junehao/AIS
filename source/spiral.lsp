;;		SPIRAL.LSP														;
;;		By June-Hao Hou, March 2006										;

(setq *spiral-inlets*
	   '("Basepoint"	   "enDPoint"		 "enDTangent"
		 "StarTPoint"	   "starTTangent"	 "Center"
		 "Radius"		   "starTAngle"		 "totaLAngle"
		 "totaLHeight"
		)
)
(setq *spiral-outlets*
	   '("enDPoint"		  "enDTangent"	   "StarTPoint"		"starTTangent"
		 "Center"		  "Radius"		   "starTAngle"		"enDAngle"
		 "totaLAngle"	  "otaLHeight"	   "NumSegments"	"Length"
		)
)

;;----------------------------------------------------------------------;
;;		Function:	C:SPIRAL											;
;;																		;
;;	Requirements:	spiral-curve										;
;;----------------------------------------------------------------------;
(defun c:spiral	(/			 pt0		 pt1		 ang0		 radius
				 total-height			 total-angle num-intervals
				 obj-type	 obj
				)
  (terpri)
  (setq pt0 (getpoint "Center point: "))
  (princ pt0)
  (terpri)
  (initget "Radius")
  (setq pt1 (getpoint pt0 "Start point [Radius and angle]: "))
  (if (or (eq pt1 "Radius"))
	(progn (terpri)
		   (setq radius (getreal "Radius: "))
		   (terpri)
		   (setq ang0 (radian (getreal "Start angle (in degree): ")))
	)
	(progn (princ pt1)
		   (setq ang0 (angle pt0 pt1))
		   (setq radius (distance pt0 pt1))
	)
  )
  (terpri)
  (setq total-height (getreal "Total height: "))
  (terpri)
  (setq total-angle (radian (getreal "Total angle (in degree): ")))
  (terpri)
  (setq num-intervals (getint "Number of segments : "))
  (terpri)
  (initget 0 "Polyline Spline")
  (setq obj-type (getint "Type of object [Polyline/Spline] <P>: "))
  (cond	((null obj-type) (setq obj-type 1))
		((or (eq obj-type "Polyline") (= obj-type 1))
		 (setq obj-type 1)
		)
		((or (eq obj-type "Spline") (= obj-type 2))
		 (setq obj-type 2)
		)
		(t (setq obj-type 1))
  )
  (spiral-curve
	pt0	ang0 radius	total-angle	total-height num-intervals obj-type)
)

;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-UPDATE										;
;;																		;
;;	Requirements:	spiral-update-spline								;
;;					spiral-update-pline3d								;
;;----------------------------------------------------------------------;
;; !!! once beein edited manually, fitpoints are gone!
(defun spiral-update (obj / ang0 rad ang tall)
  (cond	((spline-p obj) (spiral-update-spline obj))
		((pline3d-p obj) (spiral-update-pline3d obj))
  )
)


;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-UPDATE-SPLINE								;
;;																		;
;;	Requirements:	spiral-recalculate-points							;
;;					spiral-pointlist									;
;;					spiral-get-tangent-at-angle							;
;;----------------------------------------------------------------------;
;; !!! once been edited manually, fitpoints are gone!					;
(defun spiral-update-spline (obj / ang0 rad ang tall)
  (if (> (vla-get-numberoffitpoints obj) 0)
	(progn (vla-put-fitpoints
			 obj
			 (ltov
			   (spiral-recalculate-points obj)
			 )
		   )
		   (vla-put-starttangent
			 obj
			 (vlax-3d-point
			   (spiral-get-tangent-at-angle ang0 rad ang tall)
			 )
		   )
		   (vla-put-endtangent
			 obj
			 (vlax-3d-point
			   (spiral-get-tangent-at-angle ang rad ang tall)
			 )
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-UPDATE-PLINE3D								;
;;																		;
;;	Requirements:	spiral-recalculate-points							;
;;----------------------------------------------------------------------;
(defun spiral-update-pline3d (obj)
  (vla-put-coordinates obj (LtoV (spiral-recalculate-points obj))))

;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-RECALCULATE-POINTS							;
;;																		;
;;	Requirements:	spiral-pointlist									;
;;----------------------------------------------------------------------;
(defun spiral-recalculate-points (obj)
  (apply 'append
		 (spiral-pointlist
		   (get-property obj :prop-Center)
		   (get-property obj :prop-StartAngle)
		   (get-property obj :prop-Radius)
		   (get-property obj :prop-TotalAngle)
		   (get-property obj :prop-TotalHeight)
		   (get-property obj :prop-NumSegments)
		 )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-CURVE										;
;;																		;
;;	   Description: This function creates a spiral curve as a			;
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
;;	Requirements:	spiral-pointlist									;
;;					spiral-get-tangent-at-angle							;
;;																		;
;; Returned value:	a VLA object.										;
;;----------------------------------------------------------------------;
(defun spiral-curve	(ctr ang0 rad ang height num-int flag / point-list obj)
  (setq	point-list
		 (LTOV
		   (apply 'append
				  (spiral-pointlist ctr ang0 rad ang height num-int)
		   )
		 )
  )
  (cond	;; 1 -> Returns a 3DPolyline
		((= flag 1)
		 (setq obj (vla-add3dpoly (model-space) point-list))
		)
		;; 2 -> Returns a Spline curve
		((= flag 2)
		 (setq obj (vla-addSpline
					 (model-space)
					 point-list
					 ;; Start tangent
					 (vlax-3d-point
					   (spiral-get-tangent-at-angle ang0 rad ang height)
					 )
					 ;; End tangent
					 (vlax-3d-point
					   (spiral-get-tangent-at-angle (+ ang0 ang) rad ang height)
					 )
				   )
		 )
		)
  )
  (spiral-add-property obj ctr rad ang0 ang height num-int)
  obj
)


;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-POINTLIST									;
;;																		;
;;	 Description:	This function creates a spiral pointlist.			;
;;																		;
;;		Arguments:														;
;;			list	center			A point in WCS.						;
;;			float	angle0			Starting angle in radian.			;
;;			float	radius			Radius.								;
;;			float	total-angle		Total angle in radian.				;
;;			float	total-height	Total height.						;
;;			int		num-int			Number of intervals.				;
;;																		;
;;	Requirements:	cylindrical-point									;
;;																		;
;; Returned value:	a point list in the form of: 						;
;;					((x1 y1 z1) (x2 y1 z2) ...)							;
;;----------------------------------------------------------------------;
(defun spiral-pointlist	(center		  angle0	   radius		total-angle
						 total-height num-int	   /			angle-inc
						 height-inc	  index		   point-list	pt
						)
  (setq angle-inc (/ (float total-angle) num-int))
  (setq height-inc (/ (float total-height) num-int))
  (setq index 0)
  (setq point-list nil)
  (while (<= index num-int)
	(setq pt (mapcar '+
					 center
					 (cylindrical-point
					   radius
					   (+ (* index angle-inc) angle0)
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
;;		 Function:  SPIRAL-ADD-PROPERTY									;
;;----------------------------------------------------------------------;
(defun spiral-add-property (obj			  center		radius
							startAngle	  totalAngle	totalHeight
							numSegments
						   )
  (put-property obj :prop-AisObjectName "Spiral")
  (put-property obj :prop-Center center)
  (put-property obj :prop-Radius radius)
  (put-property obj :prop-StartAngle startAngle)
  (put-property obj :prop-TotalAngle totalAngle)
  (put-property obj :prop-TotalHeight totalHeight)
  (put-property obj :prop-NumSegments numSegments)
  ;; For test only...
  ;|(put-property
	obj
	"ControlPoints"
	(vla-get-controlpoints obj)
  )|;
)

;; EOF