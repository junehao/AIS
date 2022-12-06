;;	SYMBOL.LSP															;
;;	By June-Hao Hou, March 2006											;

;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR											;
;;----------------------------------------------------------------------;
(defun crosshair  ()
  (if (and *crosshair* (vla-object-p *crosshair*) (vlax-write-enabled-p *crosshair*))
	*crosshair*
	(setq *crosshair* (crosshair-new))))

;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR-NEW										;
;;----------------------------------------------------------------------;
(defun crosshair-new  ()
  (vla-addLightWeightPolyline
	(model-space)
	(vlax-list->variant
	  '(0.0 0.0 1.0 1.0 -1.0 -1.0 0.0 0.0 -1.0 1.0 1.0 -1.0))))

;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR3D-NEW										;
;;																		;
;;	Description:	This is for point mark in 3D.						;
;;----------------------------------------------------------------------;
(defun crosshair3d-new (origin / ent)
  (setq	ent	(vla-add3dpoly
			  (model-space)
			  (vlax-list->variant
				(apply 'append
					   (mapcar '(lambda (p) (vector-add origin p))
							   '((0 0 0)
								 (1 0 0)
								 (-1 0 0)
								 (0 0 0)
								 (0 1 0)
								 (0 -1 0)
								 (0 0 0)
								 (0 0 1)
								 (0 0 -1)
								)
					   )
				)
			  )
			)
  )
  (vla-scaleEntity
	ent
	(vlax-3d-point origin)
	(scale-on-screen 0.02)
  )
  (vla-put-color ent acYellow)
  ent
)


;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR-RESET										;
;;----------------------------------------------------------------------;
(defun crosshair-reset	(/ x y z)
  (setq-map '(x y z) (vlax-curve-getStartPoint (crosshair)))
  (vla-put-color (crosshair) acByLayer)
  (vla-put-coordinates (crosshair) (vlax-list->variant 
	(list x
		  y
		  (1+ x)
		  (1+ y)
		  (1- x)
		  (1- y)
		  x
		  y
		  (1- x)
		  (1+ y)
		  (1+ x)
		  (1- y)))))

;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR-MOVE										;
;;----------------------------------------------------------------------;
(defun crosshair-move  (pt)
  (vla-move	(crosshair)
			(vlax-3d-point (vlax-curve-getStartPoint (crosshair)))
			(vlax-3d-point pt)))

;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR-AUTO-SCALE								;
;;																		;
;;	Description:	Auto-scale crosshair by matching to current view.	;
;;----------------------------------------------------------------------;
(defun crosshair-auto-scale	 (/ width height ratio scale)
  (crosshair-reset)
  (vla-scaleEntity
	(crosshair)
	(vlax-3d-point (vlax-curve-getStartPoint (crosshair)))
	(scale-on-screen 0.02)))

;;----------------------------------------------------------------------;
;;		Function:	SCALE-ON-SCREEN										;
;;																		;
;;		Argument:														;
;;			float	percentage	Percentage to the screen dimension.		;
;;								0 <= percentage <= 1.					;
;;																		;
;; Returned value:	Scale factor (to be use with ScaleEntity method).	;
;;----------------------------------------------------------------------;
(defun scale-on-screen (percentage  / height ratio width)
    (setq height (getvar "viewsize")
		ratio (getvar "screensize"))
  (setq	width (* height (/ (car ratio)(cadr ratio))))
  (* percentage (min width height)))

;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR-SHOW										;
;;----------------------------------------------------------------------;
(defun crosshair-show  ()
  (crosshair-auto-scale)
  (vla-put-visible (crosshair) :vlax-true))

;;----------------------------------------------------------------------;
;;		Function:	CROSSHAIR-HIDE										;
;;----------------------------------------------------------------------;
(defun crosshair-hide  ()
  (vla-put-visible (crosshair) :vlax-false))

;; EOF