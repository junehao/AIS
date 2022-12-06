;;	POINT.LSP															;
;;	By June-Hao Hou, March 2006											;

(setq *point-inlets* '("Basepoint" "Normal" "Thickness"))
(setq *point-outlets* '("Startpoint" "Normal" "Thickness"))

;;----------------------------------------------------------------------;
;;		Function:	POINT-INTERPOLATE									;
;;----------------------------------------------------------------------;
(defun point-interpolate (parent1 parent2 param)
  (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (point-interpolate-update obj parent1 parent1 param)
	  obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	POINT-INTERPOLATE-UPDATE							;
;;----------------------------------------------------------------------;
(defun point-interpolate-update	(obj parent1 parent2 param)
  (if (and (vlax-read-enabled-p parent1)
		   (vlax-read-enabled-p parent2)
		   (vlax-write-enabled-p obj)
	  )
	(point-entmod
	  obj
	  (interpolate-to-value "Coordinates" parent1 parent2 param)
	  (interpolate-to-value "Thickness" parent1 parent2 param)
	  (interpolate-to-value "Normal" parent1 parent2 param)
	  ;; Angle of X-axis
	  0
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	POINT-ENTMOD										;
;;----------------------------------------------------------------------;
(defun point-entmod	(obj coord thickness normal angleX / changed)
  (setq changed nil)
  (if (null coord)
	(setq coord (VtoL (vla-get-coordinates obj)))
	(if	(not (equal coord (VtoL (vla-get-coordinates obj))))
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
  (if (null angleX)
	(setq angleX 0)
  )
  (if changed
	(progn (entmod
			 (point-ent-compose obj coord thickness normal angleX)
		   )
		   obj
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	POINT-ENTMOD										;
;;----------------------------------------------------------------------;
(defun point-ent-compose
	   (obj coord thickness normal angleX / ent ent-head ent-body)
  ;; Begin entity making...
  (setq ent (entget (vlax-vla-object->ename obj)))
  (setq ent-head (reverse (member (assoc 100 (reverse ent)) (reverse ent))))
  (setq	ent-body (list (cons 10 coord)
					   (cons 39 thickness)
					   (cons 210 normal)
					   (cons 50 angleX)
				 )
  )
  (append ent-head ent-body)
)


;; EOF