;;----------------------------------------------------------------------;
;;		Function:	MAKE-STAIR											;
;;																		;
;;	Description:	Generate stair by the given sequence.				;
;;					(better to store parameters for solidification in	;
;;					the sequence)										;
;;----------------------------------------------------------------------;
(defun make-stair (seq style / children)
  (if (sequence-p seq)
	(setq children (append (cons (get-property seq :prop-StartRef)
								 (seq-get-children seq)
						   )
						   (list (get-property seq :prop-Endref))
				   )
	)
  )
  (if children
	(cond ((or (null style) (eq style 'free) (= style 0))
		   (make-freesteps children)
		  )
		  ((or (eq style 'equi) (= style 1))
		   (make-equisteps children)
		  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MAKE-FREESTEPS										;
;;																		;
;;	Description:	The iterator for free-step generation.				;
;;----------------------------------------------------------------------;
(defun make-freesteps (lst)
  (if (cdr lst)
	(progn (make-freestep (car lst) (cadr lst) nil)
		   (make-freesteps (cdr lst))
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MAKE-FREESTEP										;
;;																		;
;;	Description:	Generate step by given nose and rear curves.		;
;;					Individual step rise is determined by the height	;
;;					difference of corresponding nose and rear curves.	;
;;					Suitable for landscaped stairs.						;
;;																		;
;;	Arguments:															;
;;			obj		nose		The nose curve.							;
;;			obj		rear		The rear curve.							;
;;			float	thickness	Proportion of the tread thickness.		;
;;								= 0.0 ... 1.0							;
;;																		;
;;	Returned value:	A 3DSolid object.									;
;;----------------------------------------------------------------------;
(defun make-freestep (nose		 rear		thickness  /		  rise		 nose-clone
					  rear-clone edge1		edge2	   object-list			 region
					  step
					 )
  (setq rise (- (ais:elevation rear) (ais:elevation nose)))
  (if (not thickness)
	(setq thickness 1.0)
  )
  (setq nose-clone (vla-copy nose))
  (setq rear-clone (vla-copy rear))
  (vla-move	nose-clone
			(ais:basepoint nose-clone)
			(LtoV (mapcar '+
						  (VtoL (ais:basepoint nose-clone))
						  (list 0 0 rise)
				  )
			)
  )
  (if (or (and (pline-p nose-clone)
			   (lsp-bool (vla-get-closed nose-clone))
		  )
		  (circle-p nose-clone)
	  )
	(setq region (get-region (list nose-clone)))
	(setq region (get-region
				   (list (setq edge1 (vla-addline
									   (model-space)
									   (ais:startpoint nose-clone)
									   (ais:startpoint rear-clone)
									 )
						 )
						 nose-clone
						 (setq edge2 (vla-addline
									   (model-space)
									   (ais:endpoint nose-clone)
									   (ais:endpoint rear-clone)
									 )
						 )
						 rear-clone
				   )
				 )
	)
  )
  (setq	step (vla-addextrudedsolid
			   (model-space)
			   region
			   (* rise thickness -1.0)
			   0.0
			 )
  )
  (foreach obj (list nose-clone rear-clone edge1 edge2 region)
	(if	(and obj (vla-object-p obj) (not (vlax-erased-p obj)))
	  (vla-delete obj)
	)
  )
  step
)


;;----------------------------------------------------------------------;
;;		Function:	MAKE-EQUISTEPS										;
;;																		;
;;	Description:	The iterator for equi-step generation.				;
;;----------------------------------------------------------------------;
(defun make-equisteps (lst)
  (setq start-elev (ais:elevation (car lst)))
  (setq end-elev (ais:elevation (last lst)))
  (setq min-elev (min start-elev end-elev))
  (setq max-elev (max start-elev end-elev))
  (setq total-rise (- max-elev min-elev))
  (setq num-riser (length lst))
  (setq unit-rise (/ (float total-rise) num-riser))
  (setq counter 0)
  (repeat (1- num-riser)
	(make-equistep
	  (nth counter lst)
	  (nth (1+ counter) lst)
	  (+ min-elev (* unit-rise counter))
	  unit-rise
	  nil
	)
	(setq counter (1+ counter))
  )
  ;; The end riser connect to the top slab.
  ;;(setq end (vla-copy (last lst)))
  ;;(vla-put-layer end "0")
  ;;(vla-put-color end acBylayer)
  (if (or (and (pline-p end) (lsp-bool (vla-get-closed end)))
		  (circle-p end)
	  )
	(vla-addextrudedsolid
	  (model-space)
	  (get-region (last lst))
	  (* unit-rise -1)
	  0.0
	)
	;;(vla-put-thickness end (* unit-rise -1))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MAKE-EQUISTEP										;
;;																		;
;;	Description:	Generate step by given nose and rear curves, as		;
;;					well as predetermined rise.							;
;;					Suitable for common stairs.							;
;;																		;
;;	Arguments:															;
;;			obj		nose		The nose curve.							;
;;			obj		rear		The rear curve.							;
;;			float	base-elev	Base elevation.							;
;;			float	rise		Unit rise of the step.					;
;;			float	thickness	Proportion of the tread thickness.		;
;;								= 0.0 ... 1.0 or nil.					;
;;																		;
;;	Returned value:	A 3DSolid object.									;
;;----------------------------------------------------------------------;
(defun make-equistep (nose		 rear		base-elev  rise		  thickness	 /
					  nose-clone rear-clone	edge1	   edge2	  region	 step
					 )
  (if (not thickness)
	(setq thickness 1.0)
  )
  (setq nose-clone (vla-copy nose))
  (vla-move	nose-clone
			(ais:basepoint nose-clone)
			(LtoV (append (left 2 (VtoL (ais:basepoint nose-clone)))
						  (list base-elev)
				  )
			)
  )
  (if (or (and (pline-p nose-clone)
			   (lsp-bool (vla-get-closed nose-clone))
		  )
		  (circle-p nose-clone)
	  )
	(setq object-list
		   (vlax-make-variant
			 (vlax-safearray-fill
			   (vlax-make-safearray vlax-vbObject '(0 . 0))
			   (list nose-clone)
			 )
		   )
	)
	(progn (setq rear-clone (vla-copy rear))
		   (vla-move rear-clone
					 (ais:basepoint rear-clone)
					 (LtoV (append (left 2 (VtoL (ais:basepoint rear-clone)))
								   (list base-elev)
						   )
					 )
		   )
	)
  )
  (setq	region (get-region
				 (list (setq edge1 (vla-addline
									 (model-space)
									 (ais:startpoint nose-clone)
									 (ais:startpoint rear-clone)
								   )
					   )
					   nose-clone
					   (setq edge2 (vla-addline
									 (model-space)
									 (ais:endpoint nose-clone)
									 (ais:endpoint rear-clone)
								   )
					   )
					   rear-clone
				 )
			   )
  )
  (setq	step (vla-addextrudedsolid
			   (model-space)
			   region
			   (* rise thickness)
			   0.0
			 )
  )
  (foreach obj (list nose-clone rear-clone edge1 edge2 region)
	(if	(and obj (vla-object-p obj) (not (vlax-erased-p obj)))
	  (vla-delete obj)
	)
  )
  step
)

;;----------------------------------------------------------------------;
;;		Function:	GET-REGION											;
;;																		;
;;----------------------------------------------------------------------;
(defun get-region (lst)
  (car (vlax-safearray->list
		 (vlax-variant-value
		   (vla-addregion
			 (model-space)
			 (vlax-make-variant
			   (vlax-safearray-fill
				 (vlax-make-safearray
				   vlax-vbObject
				   (cons 0 (1- (length lst)))
				 )
				 lst
			   )
			 )
		   )
		 )
	   )
  )
)