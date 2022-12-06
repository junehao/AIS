;;		POLYLINE.LSP													;
;;		By June-Hao Hou, March 2006										;

(setq *pline-inlets*
	   '("Basepoint" "ELevation" "Endpoint"	"Normal" "Startpoint" "Thickness")
)
(setq *pline-outlets*
	   '("ELevation" "Endpoint"	"Normal" "Startpoint" "Thickness" "Length")
)
(setq *pline3d-inlets* '("Basepoint" "Endpoint" "Startpoint"))
(setq *pline3d-outlets* '("Endpoint" "Startpoint" "Length"))

;;----------------------------------------------------------------------;
;;		Function:	PLINE-DERIVE										;
;;																		;
;;	Description:	Create a polyline by deriving from an existing one.	;
;;----------------------------------------------------------------------;
(defun pline-derive	(parent from to / obj)
  (setq	obj	(vla-addlightweightpolyline
			  (model-space)
			  (LToV (pline-getPropPartial 'point parent from to))
			)
  )
  (pline-setPropAll 'bulge obj (pline-getPropPartial 'bulge parent from to))
  (vla-put-elevation obj (vla-get-elevation parent))
  (vla-put-normal obj (vla-get-normal parent))
  (attach-rog obj :rog-derive (list parent from to))
  obj
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-DERIVE-UPDATE									;
;;----------------------------------------------------------------------;
(defun pline-derive-update (obj parent from to)
  (if (and (vla-object-p parent) (vlax-read-enabled-p parent) (vlax-write-enabled-p obj))
	(pline-entmod
	  obj
	  (chunk (pline-getPropPartial 'point parent from to) 2)
	  (pline-getPropPartial 'bulge parent from to)
	  (vla-get-elevation parent)
	  (vla-get-thickness parent)
	  (vtol (vla-get-normal parent))
	  (if (lsp-bool (vla-get-closed parent)) 1 0)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-DERIVE-REATTACH								;
;;----------------------------------------------------------------------;
(defun pline-derive-reattach (obj parent from to)
  (if (and (= (pline-numberOfPoints obj) (1+ (abs (- from to))))
		   (>= (min from to) 0)
		   (<= (max from to) (pline-numberOfPoints parent))
	  )
	(progn (attach-rog obj :rog-derive (list parent from to))
		   (ais-update obj)
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	PLINE-INTERPOLATE									;
;;																		;
;;	Description:	Interpolate between two polylines.					;
;;----------------------------------------------------------------------;
(defun pline-interpolate (parent1 parent2 param / obj)
  (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (pline-interpolate-update obj parent1 parent2 param)
	  obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-INTERPOLATE-UPDATE							;
;;----------------------------------------------------------------------;
(defun pline-interpolate-update	(obj parent1 parent2 param)
  (if (and (vla-object-p parent1)
		   (vla-object-p parent2)
		   (vlax-read-enabled-p parent1)
		   (vlax-read-enabled-p parent2)
		   (vlax-write-enabled-p obj)
	  )
	(pline-entmod
	  obj
	  ;; Coordinates
	  (chunk (interpolate-to-value "Coordinates" parent1 parent2 param)
			 2
	  )
	  ;; Bulge values
	  (interpolate
		(pline-getPropAll 'bulge parent1)
		(pline-getPropAll 'bulge parent2)
		param
	  )
	  (interpolate-to-value "Elevation" parent1 parent2 param)
	  (interpolate-to-value "Thickness" parent1 parent2 param)
	  (interpolate-to-value "Normal" parent1 parent2 param)
	  ;; Polyline flag
	  (if (lsp-bool (vla-get-closed parent1))
		1
		0
	  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-ENTMOD										;
;;																		;
;;	Description:	This function fully recompose the entity data of	;
;;					a LightWeightPolyline.								;
;;		Arguments:														;
;;			vla-obj	obj				A AcDbPolyline object.				;
;;			list	points			Flattened point array.				;
;;			list	bulges			List of bulge values.				;
;;			float	elevation		Elevation.							;
;;			float	thickness		Thickness.							;
;;			list	normal			A Point list.						;
;;			int		polyline-flag	0: opened, 1: closed.				;
;;																		;
;; Returned value: a list, the entity data.								;
;;----------------------------------------------------------------------;
(defun pline-entmod	(obj		 points		 bulges		 elevation
					 thickness	 normal		 polyline-flag
					 /			 changed
					)
  (setq changed nil)
  (if (null points)
	(setq points (chunk (VtoL (vla-get-coordinates obj)) 2))
	(if	(not (equal	(apply 'append points)
					(VtoL (vla-get-coordinates obj))
			 )
		)
	  (setq changed t)
	)
  )
  (if (null bulges)
	(setq bulges (pline-getPropAll 'bulge obj))
	(if	(not (equal bulges (pline-getPropAll 'bulge obj)))
	  (setq changed t)
	)
  )
  (if (null elevation)
	(setq elevation (vla-get-elevation obj))
	(if	(not (equal elevation (vla-get-elevation obj)))
	  (setq changed t)
	)
  )
  (if (null thickness)
	(setq thickness (vla-get-thickness obj))
	(if	(not (equal thickness (vla-get-thickness obj)))
	  (setq changed t)
	)
  )
  (if (null normal)
	(setq normal (VtoL (vla-get-normal obj)))
	(if	(not (equal normal (VtoL (vla-get-normal obj))))
	  (setq changed t)
	)
  )
  (if (null polyline-flag)
	(setq polyline-flag
		   (ac-bool (vla-get-closed obj))
	)
	(if (not (= polyline-flag
		   (ac-bool (vla-get-closed obj))))
	  (setq changed t))
  )
  (if changed
	(progn (entmod (pline-ent-compose
					 obj points	bulges elevation thickness normal
					 polyline-flag)
		   )
		   obj
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	PLINE-ENT-COMPOSE									;
;;----------------------------------------------------------------------;
(defun pline-ent-compose (obj		   points		bulges		 elevation
						  thickness	   normal		polyline-flag
						  /			   global-width	start-width	 end-width
						  ent		   ent-head		ent-body
						 )
  (setq	global-width 0.0
		start-width	0.0
		end-width 0.0
  )
  (setq number-of-points (length bulges))
  (setq ent (entget (vlax-vla-object->ename obj)))
  (setq ent-head (reverse (member (assoc 100 (reverse ent)) (reverse ent))))
  (setq	ent-body (append (list (cons 90 number-of-points)
							   (cons 70 polyline-flag)
							   (cons 43 global-width)
							   (cons 38 elevation)
							   (cons 39 (cdr (assoc 39 ent)))
						 )
						 ;; Point list
						 (apply	'append
								(mapcar	'(lambda (pt bg)
										   (list (cons 10 pt)
												 (cons 40 start-width)
												 (cons 41 end-width)
												 (cons 42 bg)
										   )
										 )
										points
										bulges
								)
						 )
				 )
  )
  (append (append ent-head ent-body) (list (cons 210 normal)))
)


;;----------------------------------------------------------------------;
;;		Function:	PLINE3D-INTERPOLATE									;
;;----------------------------------------------------------------------;
(defun pline3d-interpolate (parent1 parent2 param / obj)
  (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (vla-put-type obj (vla-get-type parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (pline3d-interpolate-update obj parent1 parent2 param)
	  obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE3D-INTERPOLATE-UPDATE							;
;;----------------------------------------------------------------------;
(defun pline3d-interpolate-update (obj parent1 parent2 param / coords)
  (setq	coords (interpolate
				 (vla-get-coordinates parent1)
				 (vla-get-coordinates parent2)
				 param
			   )
  )
  (if (not (equal coords (VtoL (vla-get-coordinates obj))))
	(vla-put-coordinates obj (LtoV coords))
  )
  ;;(interpolate-to-variant2 obj "Coordinates" parent1 parent2 param)
)


;;----------------------------------------------------------------------;
;;		Function:	PLINE-NUMBEROFPOINTS								;
;;----------------------------------------------------------------------;
(defun pline-numberOfPoints	(obj)
  (/ (length (vlax-variant->list (vla-get-coordinates obj)))
	 2
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-GETPROP										;
;;----------------------------------------------------------------------;
(defun pline-getProp (prop obj idx)
  (cond	((eq prop 'point)
		 (vlax-variant->list (vla-get-coordinate obj idx))
		)
		((eq prop 'bulge) (vla-getBulge obj idx))
		(T nil)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-GETPROPALL									;
;;	Description:	Get all values of a certain property.				;
;;----------------------------------------------------------------------;
(defun pline-getPropAll	(prop obj / lst idx)
  (cond	((eq prop 'point)
		 (vlax-variant->list (vla-get-coordinates obj))
		)
		((eq prop 'bulge)
		 (setq lst nil
			   idx (pline-numberOfPoints obj)
		 )
		 (while	(> idx 0)
		   (setq idx (1- idx)
				 lst (cons (pline-getProp 'bulge obj idx) lst)
		   )
		 )
		)
		(T nil)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-GETPROPPARTIAL								;
;;----------------------------------------------------------------------;
(defun pline-getPropPartial	(prop obj from to / lst)
  (setq lst (pline-getPropAll prop obj))
  (if (eq 'point prop)		  ; slice point list into chunks
	(setq lst (chunk lst 2))
  )
  (setq	lst	(if	(<= from to)
			  (sublist lst from to)
			  (reverse (sublist lst to from))
			)
  )
  (if (eq 'point prop)
	(apply 'append lst)
	lst
  )
)


;;----------------------------------------------------------------------;
;;		Function:	PLINE-SETPROP										;
;;----------------------------------------------------------------------;
(defun pline-setProp (prop obj idx val)
  (cond	((eq prop 'point)
		 (vla-put-coordinate
		   obj
		   idx
		   (left 2 (vlax-list->variant val))
		 )
		)
		((eq prop 'bulge) (vla-setBulge obj idx val))
		(T nil)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-SETPROPALL									;
;;----------------------------------------------------------------------;
(defun pline-setPropAll	(prop obj lst / idx)
  (cond	((eq prop 'point)
		 ;;(princ " (points)")
		 (vla-put-coordinates obj (vlax-list->variant lst))
		)
		((eq prop 'bulge)
		 ;;(princ " (bulges)")
		 (setq idx 0)
		 (repeat (pline-numberOfPoints obj)
		   ;;(pline-setProp prop obj idx (nth idx lst))
		   (vla-setBulge obj idx (nth idx lst))
		   (setq idx (1+ idx))
		 )
		)
		(T nil)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-SETPROPPARTIAL								;
;;----------------------------------------------------------------------;
(defun pline-setPropPartial	(prop obj from to lst)
  (cond	((eq prop 'point) nil)
		(T nil)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-SETBASEPOINT									;
;;																		;
;;	Description:	Rotate basepoint of a closed polyline.				;
;;----------------------------------------------------------------------;
(defun pline-setBasepoint (obj n / points bulges numPts)
  (if (lsp-bool (vla-get-closed obj))
	(progn (setq points	(chunk (pline-getPropAll 'point obj) 2)
				 bulges	(pline-getPropAll 'bulge obj)
				 numPts	(- (pline-numberOfPoints obj) n)
		   )
		   (pline-entmod
			 obj
			 (append (right numPts points) (left n points))
			 (append (right numPts bulges) (left n bulges))
			 nil
			 nil
			 nil
			 nil
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-REVERSE										;
;;																		;
;;	Description:	Reverse point order.								;
;;----------------------------------------------------------------------;
(defun pline-reverse (obj / bulges-old points bulges)
  (setq bulges-old (pline-getPropAll 'bulge obj))
  (setq	points (reverse (chunk (pline-getPropAll 'point obj) 2))
		bulges (mapcar '(lambda (a) (* -1 a))
					   (append (cdr (reverse bulges-old))
							   (list (car (reverse bulges-old)))
					   )
			   )
  )
  (pline-entmod obj points bulges nil nil nil nil)
)


;;----------------------------------------------------------------------;
;;		Function:	PLINE-ADDVERTEX										;
;;																		;
;;	Description:	Add vertex pt after the index.						;
;;----------------------------------------------------------------------;
(defun pline-addVertex (obj idx pt / pt2 sa va)
  (vla-addVertex obj idx (LtoV (left 2 pt)))
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-POINTNUMBER-SHOW								;
;;----------------------------------------------------------------------;
(defun pline-pointnumber-show
	   (obj / layer layer-save idx elev labels scale loc txtobj)
  (setq	layer	   (layer-by-name "AIS-Label")
		layer-save (active-layer)
		idx		   0
		elev	   (vla-get-elevation obj)
		labels	   nil
		scale	   (* 0.02 (getvar "viewsize"))
  )
  (if (assoc obj *AIS-Labels*)
	(pline-pointnumber-hide obj)
  )
  (layer-setActive layer)
  (while (< idx (pline-numberOfPoints obj)) ; Draw labels
	(setq loc (pline-getProp 'point obj idx))
	(setq txtobj (vla-addText (model-space) (itoa idx) (vlax-3d-point loc) 1))
	(vla-scaleEntity txtobj (vlax-3d-point loc) scale)
	(setq labels (append labels (list txtobj))
		  idx	 (1+ idx)
	)
  )
  (setq *AIS-Labels* (append *AIS-Labels* (list (cons obj labels))))
  (layer-setActive layer-save)
)

;;----------------------------------------------------------------------;
;;		Function:	PLINE-POINTNUMBER-HIDE								;
;;----------------------------------------------------------------------;
(defun pline-pointnumber-hide (obj)
  (mapcar '(lambda (a)
			 (if (not (vlax-erased-p a))
			   (vla-delete a)
			 )
		   )
		  (cdr (assoc obj *AIS-Labels*))
  )
  (setq *AIS-Labels* (vl-remove (assoc obj *AIS-Labels*) *AIS-Labels*))
  (princ)
)

;; EOF