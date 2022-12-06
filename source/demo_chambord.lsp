(defun demo-chambord (flights	/		  innerPath	outerPath startRef	endRef
					  landing	prevEnd	  prevPath	prevPath2 center	ang
					  hi		seg		  typ		innerR	  outerR	flight
					 )
  (command-begin)
  (setq	ang	   (radian 70)
		hi	   160
		seg	   7
		typ	   1
		innerR 200
		outerR 280
		bg	   (bulge (radian 20))
  )
  (if (> flights 10)
	(setq flights 10)
  )
  (if (or (zerop flights) (minusp flights))
	(setq flights 1)
  )
  (foreach angles '(0 90 180 270)
	(setq prevEnd nil)
	(setq flight 0)
	;; For each quadrant, generate a complete N-flight spiral stair
	(repeat	flights
	  (setq center (list 0 0 (* flight hi)))
	  (setq ang0 (radian (+ 10 angles (* flight 90))))
	  ;; Spiral curves
	  (setq innerPath (spiral-curve center ang0 innerR ang hi seg typ))
	  (setq outerPath (spiral-curve center ang0 outerR ang hi seg typ))
	  ;; StartRef and EndRef
	  (setq	startRef (vla-addline
					   (model-space)
					   (vlax-3d-point (vlax-curve-getstartpoint innerPath))
					   (vlax-3d-point (vlax-curve-getstartpoint outerPath))
					 )
	  )
	  (setq	endRef (vla-addline
					 (model-space)
					 (vlax-3d-point (vlax-curve-getendpoint innerPath))
					 (vlax-3d-point (vlax-curve-getendpoint outerPath))
				   )
	  )
	  ;; If previous sequence exists, then connect by a landing
	  (if prevEnd
		(progn ;; Create the landing boundary
			   (setq landing (vla-addlightweightpolyline
							   (model-space)
							   (LtoV
								 (apply	'append
										(list (left 2 (VtoL (vla-get-startpoint prevEnd)))
											  (left 2 (VtoL (vla-get-endpoint prevEnd)))
											  (left 2 (VtoL (vla-get-endpoint startRef)))
											  (left 2 (VtoL (vla-get-startpoint startRef)))
										)
								 )
							   )
							 )
			   )
			   (vla-put-closed landing acTrue)
			   (vla-put-elevation landing (ais:elevation startRef))
			   (vla-setbulge landing 1 bg)
			   (vla-setbulge landing 3 (* bg -1))
			   ;; Bind 4 curves to the landing
			   (bind prevPath
					 "endpoint"
					 (list 'ais:point-on-curve-at-param landing 0)
					 t
			   )
			   (bind innerPath
					 "basepoint"
					 (list 'ais:point-on-curve-at-param landing 3)
					 t
			   )
			   (bind prevPath2
					 "endpoint"
					 (list 'ais:point-on-curve-at-param landing 1)
					 t
			   )
			   (bind outerPath
					 "basepoint"
					 (list 'ais:point-on-curve-at-param landing 2)
					 t
			   )
		)
	  )
	  ;; Create sequence
	  (add-sequence innerPath startRef endRef 14)
	  ;; Bind child entities to the 2nd Path
	  (foreach child (seq-get-children innerPath)
		(bind child
			  "endpoint"
			  (list 'ais:point-on-curve-at-ppt outerPath 'myseqparam)
			  nil
		)
	  )
	  ;; Redraw stair sequence
	  (ais-touch innerPath)
	  ;; To be used by next sequence
	  (setq prevEnd endRef)
	  (setq prevPath innerPath)
	  (setq prevPath2 outerPath)
	  (setq flight (1+ flight))
	)
  )
  (command-end)
  (command "zoom" "all")
)