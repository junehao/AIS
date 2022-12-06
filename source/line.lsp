;;		LINE.LSP														;
;;		By June-Hao Hou, March, 2006									;
(setq *line-inlets* '("Basepoint" "Endpoint" "Normal" "Startpoint" "Thickness"))
(setq *line-outlets* '("Endpoint" "Normal" "Startpoint" "Thickness" "Length"))

;;----------------------------------------------------------------------;
;;		Function:	LINE-DERIVE											;
;;----------------------------------------------------------------------;
(defun line-derive (parent from to)
  nil)

;;----------------------------------------------------------------------;
;;		Function:	LINE-DERIVE-UPDATE									;
;;----------------------------------------------------------------------;
(defun line-derive-update (obj parent from to)
  nil)

;;----------------------------------------------------------------------;
;;		Function:	LINE-INTERPOLATE									;
;;----------------------------------------------------------------------;
(defun line-interpolate	(parent1 parent2 param / obj)
  (if (object-identical-p parent1 parent2)
	(progn (setq obj (vla-copy parent1))
		   (attach-rog
			 obj
			 :rog-interpolate
			 (list parent1 parent2 param)
		   )
		   (line-interpolate-update obj parent1 parent1 param)
		   obj
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	LINE-INTERPOLATE-UPDATE								;
;;----------------------------------------------------------------------;
(defun line-interpolate-update (obj parent1 parent2 param)
  (if (and (vlax-read-enabled-p parent1)
		   (vlax-read-enabled-p parent2)
		   (vlax-write-enabled-p obj)
	  )
	(line-entmod
	  obj
	  (interpolate-to-value "StartPoint" parent1 parent2 param)
	  (interpolate-to-value "EndPoint" parent1 parent2 param)
	  (interpolate-to-value "Normal" parent1 parent2 param)
	  (interpolate-to-value "Thickness" parent1 parent2 param)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	LINE-ENTMOD											;
;;----------------------------------------------------------------------;
(defun line-entmod (obj start end normal thickness / changed)
  (setq changed nil)
  (if (null start)
	(setq start (VtoL (vla-get-startpoint obj)))
	(if	(not (equal start (VtoL (vla-get-startpoint obj))))
	  (setq chanegd t)
	)
  )
  (if (null end)
	(setq end (VtoL (vla-get-endpoint obj)))
	(if	(not (equal end (VtoL (vla-get-endpoint obj))))
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
  (if changed
	(progn (entmod (line-ent-compose obj start end normal thickness))
		   obj
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	LINE-ENT-COMPOSE									;
;;----------------------------------------------------------------------;
(defun line-ent-compose	(obj start end normal thickness / ent ent-head ent-body)
  (setq ent (entget (vlax-vla-object->ename obj)))
  (setq ent-head (reverse (member (assoc 100 (reverse ent)) (reverse ent))))
  (setq	ent-body (list (cons 39 thickness)
					   (cons 10 start)
					   (cons 11 end)
					   (cons 210 normal)
				 )
  )
  (append ent-head ent-body)
)
;; EOF