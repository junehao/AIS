;;	VLA-LSP																;
;;	By June-Hao Hou, March 2006											;
(vl-load-com)

;;//////////////////////////////////////////////////////////////////////;
;;		AutoCAD DOM	& VLA Extension										;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	ACAD-OBJECT											;
;;																		;
;; 	 Description:	Returns the (cached) ACAD application object		;
;;----------------------------------------------------------------------;
(defun acad-object ()
  (cond	(*acad-object*)
		(T (setq *acad-object* (vlax-get-acad-object)))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ACTIVE-DOCUMENT										;
;;																		;
;; 	 Description:	Returns the (cached) active document object.		;
;;----------------------------------------------------------------------;
(defun active-document ()
  (cond	(*active-document*)
		(T
		 (setq *active-document* (vla-get-activedocument (acad-object)))
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MODEL-SPACE											;
;;																		;
;;	 Description:	Returns the (cached) model space object.			;
;;----------------------------------------------------------------------;
(defun model-space ()
  (cond	(*model-space*)
		(T
		 (setq *model-space* (vla-get-modelspace (active-document)))
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	OBJSEL												;
;;----------------------------------------------------------------------;
(defun objsel ()
  (initget 0)
  (if (setq ent (entsel "Pick an object:"))
	(vlax-ename->vla-object (car ent))
  )
)


;;----------------------------------------------------------------------;
;;		Function:	VLAX-VARIANT->LIST									;
;;																		;
;; Returned value:	a list												;
;;----------------------------------------------------------------------;
(defun vlax-variant->list (var)
  (vlax-safearray->list (vlax-variant-value var))
)

;;----------------------------------------------------------------------;
;;		Function:	VTOL (= VLAX-VARIANT->LIST)							;
;;----------------------------------------------------------------------;
(defun vtol	(var)
  (vlax-safearray->list (vlax-variant-value var))
)

;;----------------------------------------------------------------------;
;;		Function:	VLAX-LIST->VARIANT									;
;;																		;
;;	Requirements:	vlax-list->safearray								;
;;																		;
;; Returned value:	a variant (array)									;
;;----------------------------------------------------------------------;
(defun vlax-list->variant (lst)
  (vlax-make-variant
	(vlax-list->safearray lst)
	(+ vlax-vbArray vlax-vbDouble)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	LTOV (= VLAX-LIST->VARIANT)							;
;;----------------------------------------------------------------------;
(defun ltov	(lst)
  (vlax-make-variant
	(vlax-list->safearray lst)
	(+ vlax-vbArray vlax-vbDouble)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	VLAX-VARIANT->SAFEARRAY								;
;;																		;
;; Returned value:	a safearray											;
;;----------------------------------------------------------------------;
(defun vlax-variant->safearray (var)
  (vlax-variant-value var)
)

;;----------------------------------------------------------------------;
;;		Function:	VLAX-LIST->SAFEARRAY								;
;;																		;
;; Returned value:	a safearray											;
;;----------------------------------------------------------------------;
(defun vlax-list->safearray	(lst)
  (if (and lst (> (length lst) 0))
	(vlax-safearray-fill
	  (vlax-make-safearray
		vlax-vbDouble
		(cons 0 (fix (1- (length lst))))
	  )
	  lst
	)
  )
)

;;//////////////////////////////////////////////////////////////////////;
;;		Predicate Functions												;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	VLA-OBJECT-P										;
;;----------------------------------------------------------------------;
(defun vla-object-p (var) (eq 'VLA-OBJECT (type var)))

;;----------------------------------------------------------------------;
;;		Function:	PATH-P												;
;;----------------------------------------------------------------------;
(defun path-p (obj)
  (and (vla-object-p obj)
	   (or (arc-p obj)
		   (circle-p obj)
		   (line-p obj)
		   (pline-p obj)
		   (pline3d-p obj)
		   (spline-p obj)
	   )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-P												;
;;----------------------------------------------------------------------;
(defun arc-p (var) (eq "AcDbArc" (vla-get-objectname var)))

;;----------------------------------------------------------------------;
;;		Function:	CIRCLE-P											;
;;----------------------------------------------------------------------;
(defun circle-p	(var)
  (eq "AcDbCircle" (vla-get-objectname var))
)

;;----------------------------------------------------------------------;
;;		Function:	ELLIPSE-P											;
;;----------------------------------------------------------------------;
(defun ellipse-p (var)
  (eq "AcDbEllipse" (vla-get-objectname var))
)

;;----------------------------------------------------------------------;
;;		Function:	LINE-P												;
;;----------------------------------------------------------------------;
(defun line-p (var)
  (eq "AcDbLine" (vla-get-objectname var))
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-P												;
;;----------------------------------------------------------------------;
(defun pline-p (var)
  (or (eq "AcDbPolyline" (vla-get-objectname var))
	  (eq "AcDb2dPolyline" (vla-get-objectname var))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE3D-P											;
;;----------------------------------------------------------------------;
(defun pline3d-p (var)
  (eq "AcDb3dPolyline" (vla-get-objectname var))
)

;;----------------------------------------------------------------------;
;;		Function:	POINT-P												;
;;----------------------------------------------------------------------;
(defun point-p (var)
  (eq "AcDbPoint" (vla-get-objectname var))
)

;;----------------------------------------------------------------------;
;;		Function:	SPIRAL-P											;
;;----------------------------------------------------------------------;
(defun spiral-p	(var)
  (and (or (spline-p var) (pline3d-p obj))
	   (eq (vlax-ldata-get var "AisObjectName") "Spiral")
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SPLINE-P											;
;;----------------------------------------------------------------------;
(defun spline-p	(var)
  (eq "AcDbSpline" (vla-get-objectname var))
)

;;----------------------------------------------------------------------;
;;		Function:	VARIANT-P											;
;;----------------------------------------------------------------------;
(defun variant-p (var) (eq 'variant (type var)))

;;----------------------------------------------------------------------;
;;		Function:	OBJECT-IDENTICAL-P									;
;;----------------------------------------------------------------------;
(defun object-identical-p (obj1 obj2)
  (if (eq (vla-get-objectname obj1) (vla-get-objectname obj2))
	(cond ((or (pline-p obj1) (pline3d-p obj1))
		   (= (ais:number-of-points obj1) (ais:number-of-points obj2))
		  )
		  ((or (point-p obj1)
			   (line-p obj1)
			   (circle-p obj1)
			   (arc-p obj1)
			   (ellipse-p obj1)
		   )
		   t
		  )
		  ((spline-p obj1)
		   (and	(= (vla-get-numberoffitpoints obj1)
				   (vla-get-numberoffitpoints obj2)
				)
				(= (vla-get-numberofcontrolpoints obj1)
				   (vla-get-numberofcontrolpoints obj2)
				)
		   )
		  )
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	BASEPOINT											;
;;----------------------------------------------------------------------;
(defun basepoint (obj)
  (if (and obj (vla-object-p obj))
	(cond ((point-p obj) (VtoL (vla-get-coordinates obj)))
		  ((or (line-p obj)
			   (pline-p obj)
			   (pline3d-p obj)
			   (spline-p obj)
		   )
		   (vlax-curve-getStartPoint obj)
		  )
		  ((or (arc-p obj) (circle-p obj) (ellipse-p obj))
		   (VtoL (vla-get-center obj))
		  )
		  (T (alert "can't get basepoint of this object!"))
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	INTERPOLATE-TO-VARIANT								;
;;----------------------------------------------------------------------;
(defun interpolate-to-variant (prop parent1 parent2 param)
  (ltov	(interpolate
		  (vlax-get-property parent1 prop)
		  (vlax-get-property parent2 prop)
		  param
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	INTERPOLATE-TO-VALUE								;
;;----------------------------------------------------------------------;
(defun interpolate-to-value	(prop parent1 parent2 param)
  (interpolate
	(vlax-get-property parent1 prop)
	(vlax-get-property parent2 prop)
	param
  )
)

;;----------------------------------------------------------------------;
;;		Function:	INTERPOLATE-TO-VARIANT2								;
;;----------------------------------------------------------------------;
(defun interpolate-to-variant2 (obj prop parent1 parent2 param)
  (vlax-put-property
	obj
	prop
	(LtoV (interpolate
			(vlax-get-property parent1 prop)
			(vlax-get-property parent2 prop)
			param
		  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	INTERPOLATE-TO-VALUE2								;
;;----------------------------------------------------------------------;
(defun interpolate-to-value2 (obj prop parent1 parent2 param)
  (vlax-put-property
	obj
	prop
	(interpolate
	  (vlax-get-property parent1 prop)
	  (vlax-get-property parent2 prop)
	  param
	)
  )
)


;; EOF