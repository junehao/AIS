;;	SPLINE.LSP															;
;;																		;

(setq *spline-inlets*
	   '("Basepoint" "enDPoint"	"enDTangent" "StarTPoint" "starTTangent")
)
(setq *spline-outlets*
	   '("enDPoint"	"enDTangent" "StarTPoint" "starTTangent" "Length")
)

;;----------------------------------------------------------------------;
;;		Function:	SPLINE-INTERPOLATE									;
;;																		;
;;	Description:	Interpolate between two splines.					;
;;----------------------------------------------------------------------;
(defun spline-interpolate (parent1 parent2 param)
  (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (spline-interpolate-update obj parent1 parent2 param)
		   obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SPLINE-INTERPOLATE-UPDATE							;
;;----------------------------------------------------------------------;
(defun spline-interpolate-update (obj parent1 parent2 param)
  (if (and (vla-object-p parent1)
		   (vla-object-p parent2)
		   (vlax-read-enabled-p parent1)
		   (vlax-read-enabled-p parent2)
		   (vlax-write-enabled-p obj)
	  )
	(progn (if (> (vla-get-numberoffitpoints obj) 0)
			 (interpolate-to-variant2
			   obj "FitPoints" parent1 parent2 param)
			 (interpolate-to-variant2
			   obj "ControlPoints" parent1 parent2 param)
		   )
		   (interpolate-to-variant2
			 obj "StartTangent"	parent1	parent2	param)
		   (interpolate-to-variant2
			 obj "EndTangent" parent1 parent2 param)
	)
  )
)


;; Get distance at the given fit point.
;; @param	VLA object.
;; @param	Integer. Index of the fit point.
;; @return	Distance.
;;
(defun spline-getDistAtFitPoint	(obj index)
  (if (>= index (1- (vla-get-numberOfFitPoints obj)))
	;;(vlax-curve-getLength obj)
	(vlax-curve-getDistAtPoint
	  obj
	  (vlax-3d-point->list (vla-getFitPoint obj index))
	)
  )
)

;; Insert a new fit point between two existing ones.
;; @param	VLA object.
;; @param	Double. The index (based on fit points) to which
;;		new fit point is inserted.
;; @return	Variant. The new fit point.
;;
(defun spline-insertFitPoint (obj index / numFitPoints point)
  (setq numFitPoints (vla-get-numberOfFitPoints obj))
  (if (or (<= index 0)
		  (>= index (1- numFitPoints))
		  (= index (fix index))
	  )
	(*error* "Index is a fit point or is out of range!")
	(progn (setq point (vlax-3D-point (vlax-spline-getPointAtFPIndex obj index)))
		   (vla-addFitPoint obj (ceiling index) point)
	)
  )
)

;; Returns a point at the index according to fit point position.
;; @param	VLA object
;; @param	Double. Index based on fit points
;; @return	Variant. A 3D point.
;;
(defun spline-getPointAtFPIndex	(obj index / lowDist upDist dist)
  (if (= index (fix index))
	(vlax-3d-point->list (vla-getFitPoint obj index))
	(progn (setq lowDist (vlax-spline-getDistAtFitPoint obj (fix index)))
		   (setq upDist (vlax-spline-getDistAtFitPoint obj (ceiling index)))
		   (setq dist (+ lowDist (* (- upDist lowDist) (- index (fix index)))))
		   (vlax-curve-getPointAtDist obj dist)
	)
  )
)

;; EOF