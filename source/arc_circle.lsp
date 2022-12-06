;;		ARC_CIRCLE.LSP													;
;;		By June-Hao Hou, March 2006										;

(setq *arc-inlets*
	   '("Basepoint" "Center" "Endangle" "Normal" "Radius" "Startangle"
		 "Thickness")
)
(setq *arc-outlets*
	   '("Center"		"enDAngle"	   "Normal"		  "Radius"
		 "starTAngle"	"THickness"	   "Arclength"	  "enDPoint"
		 "Length"		"starTPoint"
		)
)
(setq *circle-inlets* '("Basepoint" "Center" "Normal" "Radius" "Thickness"))
(setq *circle-outlets*
	   '("CEnter" "Normal" "Radius"	"Thickness"	"Diameter" "CIrcumference")
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-INTERPOLATE										;
;;----------------------------------------------------------------------;
(defun arc-interpolate (parent1 parent2 param / obj)
  (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (arc-interpolate-update obj parent1 parent1 param)
		   obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-INTERPOLATE-UPDATE								;
;;----------------------------------------------------------------------;
(defun arc-interpolate-update (obj parent1 parent2 param)
  (if (and (vlax-read-enabled-p parent1)
		   (vlax-read-enabled-p parent2)
		   (vlax-write-enabled-p obj)
	  )
	(arc-entmod
	  obj
	  (interpolate-to-value "Center" parent1 parent2 param)
	  (interpolate-to-value "Radius" parent1 parent2 param)
	  (interpolate-to-value "StartAngle" parent1 parent2 param)
	  (interpolate-to-value "EndAngle" parent1 parent2 param)
	  (interpolate-to-value "Normal" parent1 parent2 param)
	  (interpolate-to-value "Thickness" parent1 parent2 param)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-DERIVE											;
;;																		;
;;		Arguments:														;
;;			vla-obj	parent	An arc or circle object.					;
;;			float	from	Start angle.								;
;;			float	to		End angle.									;
;;																		;
;; Returned value:	An ARC object.										;
;;----------------------------------------------------------------------;
(defun arc-derive (parent from to)
  (if (and (vla-object parent)
		   (or (circle-p parent) (arc-p parent))
	  )
	(vla-addArc
	  (model-space)
	  (vla-get-center parent)
	  (vla-get-radius parent)
	  from
	  to
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-DERIVE-UPDATE									;
;;----------------------------------------------------------------------;
(defun arc-derive-update (obj parent from to)
  (if (and (vlax-read-enabled-p parent)
		   (vlax-write-enabled-p obj)
	  )
	(arc-entmod
	  obj
	  (VtoL (vla-get-center parent))
	  (vla-get-radius parent)
	  from
	  to
	  (VtoL (vla-get-normal parent))
	  (vla-get-thickness parent)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-DERIVE-PLINE									;
;;																		;
;;	Description:	Without two-way coordinate transformations, this	;
;;					function only works properly on XY-plane.			;
;;----------------------------------------------------------------------;
(defun arc-derive-pline	(parent from to / obj ang)
  (if (null from)
	(setq from (vla-get-startangle parent))
  )
  (if (null to)
	(setq to (vla-get-endangle parent))
  )
  (setq	obj	(vla-addlightweightpolyline
			  (model-space)
			  (LtoV	(append	(left 2 (vlax-curve-getstartpoint parent))
							(left 2 (vlax-curve-getendpoint parent))
					)
			  )
			)
  )
  (if (> from to)
	(setq ang (- (* 2 pi) (- from to)))
	(setq ang (- to from))
  )
  (vla-setbulge obj 0 (bulge ang))
  (vla-put-normal obj (vla-get-normal parent))
  (vla-put-thickness obj (vla-get-thickness parent))
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-ENTMOD											;
;;----------------------------------------------------------------------;
(defun arc-entmod (obj center radius start-ang end-ang normal thickness	/
				   changed)
  (setq changed nil)
  ;; Fill in the empty values and validate
  (if (null center)
	(setq center (VtoL (vla-get-center obj)))
	(if	(not (equal center (VtoL (vla-get-center obj))))
	  (setq changed t)
	)
  )
  (if (null radius)
	(setq radius (vla-get-radius obj))
	(if	(not (equal radius (vla-get-radius obj)))
	  (setq changed t)
	)
  )
  (if (null start-ang)
	(setq start-ang (vla-get-startangle obj))
	(if	(not (equal start-ang (vla-get-startangle obj)))
	  (setq changed t)
	)
  )
  (if (null end-ang)
	(setq end-ang (vla-get-endangle obj))
	(if	(not (equal end-ang (vla-get-endangle obj)))
	  (setq changed t)
	)
  )
  (if (null normal)
	(setq normal (VtoL (vla-get-normal obj)))
	(if	(not (equal normal (VtoL (vla-get-normal obj))))
	  (setq changed t)
	)
  )
  (if (null thickness)
	(setq thickness (vla-get-thickness obj))
	(if	(not (equal thickness (vla-get-thickness obj)))
	  (setq changed t)
	)
  )
  ;; Modify entity data if needed...
  (if changed
	(progn (entmod (arc-ent-compose
					 obj	center radius start-ang	end-ang	normal thickness)
		   )
		   obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ARC-ENT-COMPOSE										;
;;----------------------------------------------------------------------;
(defun arc-ent-compose (obj		  center	radius	  start-ang	end-ang
						normal	  thickness	/		  ent		ent1
						ent-head  ent-body
					   )
  (setq ent (entget (vlax-vla-object->ename obj)))
  (setq ent1 (member (assoc 100 (reverse ent)) (reverse ent)))
  (setq ent-head (reverse (member (assoc 100 ent1) ent1)))
  (setq	ent-body (list (cons 39 thickness)
					   (cons 10 center)
					   (cons 40 radius)
					   (cons 210 normal)
					   (cons 100 "AcDbArc")
					   (cons 50 start-ang)
					   (cons 51 end-ang)
				 )
  )
  (append ent-head ent-body)
)

;;----------------------------------------------------------------------;
;;		Function:	CIRCLE-INTERPOLATE									;
;;----------------------------------------------------------------------;
(defun circle-interpolate (parent1 parent2 param / obj)
  (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (circle-interpolate-update obj parent1 parent2 param)
		   obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CIRCLE-INTERPOLATE-UPDATE							;
;;----------------------------------------------------------------------;
(defun circle-interpolate-update (obj parent1 parent2 param)
  (if (and (vlax-read-enabled-p parent1)
		   (vlax-read-enabled-p parent2)
		   (vlax-write-enabled-p obj)
	  )
	(circle-entmod
	  obj
	  (interpolate-to-value "Center" parent1 parent2 param)
	  (interpolate-to-value "Radius" parent1 parent2 param)
	  (interpolate-to-value "Normal" parent1 parent2 param)
	  (interpolate-to-value "Thickness" parent1 parent2 param)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CIRCLE-DERIVE										;
;;																		;
;;		Arguments:														;
;;			vla-obj	parent		Circle object.							;
;;			float	from		Start angle.							;
;;			float	to			End angle.								;
;;																		;
;; Returned value:	An ARC or CIRCLE object.							;
;;----------------------------------------------------------------------;
(defun circle-derive (parent from to)
  (if (and (vla-object-p parent) (circle-p parent))
	(if	(or (= from to) (and (null from) (null to)))
	  (vla-copy parent)
	  (vla-addArc
		(model-space)
		(vla-get-center parent)
		(vla-get-radius parent)
		from
		to
	  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CIRCLE-DERIVE-PLINE									;
;;----------------------------------------------------------------------;
(defun circle-derive-pline (parent from to / obj ang)
  (setq	obj	(vla-addlightweightpolyline
			  (model-space)
			  (LtoV	(append	(vlax-curve-getstartpoint parent)
							(vlax-curve-getendpoint parent)
					)
			  )
			)
  )
  (if (> from to)
	(setq ang (- (* 2 pi) (- from to)))
	(setq ang (- to from))
  )
  (vla-setbulge obj 0 (bulge ang))
)

;;----------------------------------------------------------------------;
;;		Function:	CIRCLE-ENTMOD										;
;;----------------------------------------------------------------------;
(defun circle-entmod (obj center radius normal thickness / changed)
  (setq changed nil)
  (if (null center)
	(setq center (VtoL (vla-get-center obj)))
	(if	(not (equal center (VtoL (vla-get-center obj))))
	  (setq changed t)
	)
  )
  (if (null radius)
	(setq radius (vla-get-radius obj))
	(if	(not (equal radius (vla-get-radius obj)))
	  (setq changed t)
	)
  )
  (if (null normal)
	(setq normal (VtoL (vla-get-normal obj)))
	(if	(not (equal normal (VtoL (vla-get-normal obj))))
	  (setq changed t)
	)
  )
  (if (null thickness)
	(setq thickness (vla-get-thickness obj))
	(if	(not (equal thickness (vla-get-thickness obj)))
	  (setq changed t)
	)
  )
  ;; Begin entity making...
  (if changed
	(progn (entmod
			 (circle-ent-compose obj center radius normal thickness)
		   )
		   obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CIRCLE-ENT-COMPOSE									;
;;----------------------------------------------------------------------;
(defun circle-ent-compose
	   (obj center radius normal thickness / ent ent-head ent-body)
  (setq ent (entget (vlax-vla-object->ename obj)))
  (setq ent-head (reverse (member (assoc 100 (reverse ent)) (reverse ent))))
  (setq	ent-body (list (cons 39 thickness)
					   (cons 10 center)
					   (cons 40 radius)
					   (cons 210 normal)
				 )
  )
  (append ent-head ent-body)
)


;;----------------------------------------------------------------------;
;;		Function:	ELLIPSE-INTERPOLATE									;
;;----------------------------------------------------------------------;
(defun ellipse-interpolate (parent1 parent2 param)
   (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (ellipse-interpolate-update obj parent1 parent2 param)
		   obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ELLIPSE-INTERPOLATE-UPDATE							;
;;----------------------------------------------------------------------;
(defun ellipse-interpolate-update (obj parent1 parent2 param)
  (if (and (vlax-read-enabled-p parent1)
		   (vlax-read-enabled-p parent2)
		   (vlax-write-enabled-p obj)
	  )
	(ellipse-entmod
	  obj
	  (interpolate-to-value "Center" parent1 parent2 param)
	  (interpolate-to-value "MajorAxis" parent1 parent2 param)
	  (interpolate-to-value "Normal" parent1 parent2 param)
	  (interpolate-to-value "RadiusRatio" parent1 parent2 param)
	  (interpolate-to-value "StartParam" parent1 parent2 param)
	  (interpolate-to-value "EndParam" parent1 parent2 param)
	)
  )
)


(defun ellipse-entmod (obj center majoraxis normal ratio startparam endparam)
  (setq changed nil)
  (if (null center)
	(setq center (VtoL (vla-get-center obj)))
	(if	(not (equal center (VtoL (vla-get-center obj))))
	  (setq changed t)
	)
  )
  (if (null majoraxis)
	(setq majoraxis (VtoL (vla-get-majoraxis obj)))
	(if	(not (equal majoraxis (VtoL (vla-get-majoraxis obj))))
	  (setq changed t)
	)
  )
  (if (null normal)
	(setq normal (VtoL (vla-get-normal obj)))
	(if	(not (equal normal (VtoL (vla-get-normal obj))))
	  (setq changed t)
	)
  )
  (if (null ratio)
	(setq ratio (vla-get-radiusratio obj))
	(if	(not (equal ratio (vla-get-radiusratio obj)))
	  (setq changed t)
	)
  )
  (if (null startparam)
	(setq startparam (vla-get-startparameter obj))
	(if	(not (equal startparam (vla-get-startparameter obj)))
	  (setq changed t)
	)
  )
  (if (null endparam)
	(setq endparam (vla-get-endparameter obj))
	(if	(not (equal endparam (vla-get-endparameter obj)))
	  (setq changed t)
	)
  )
  ;; Begin entity making...
  (if changed
	(progn (entmod
			 (ellipse-ent-compose obj center majoraxis normal ratio startparam endparam)
		   )
		   obj
	)
  )
)

(defun ellipse-ent-compose (obj center majoraxis normal ratio startparam endparam / ent ent-head ent-body)
  (setq ent (entget (vlax-vla-object->ename obj)))
  (setq ent-head (reverse (member (assoc 100 (reverse ent)) (reverse ent))))
  (setq	ent-body (list (cons 10 center)
					   (cons 11 majoraxis)
					   (cons 210 normal)
					   (cons 40 ratio)
					   (cons 41 startparam)
					   (cons 42 endparam)
				 )
  )
  (append ent-head ent-body)
)