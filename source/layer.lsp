;;		LAYER.LSP														;
;; 		By June-Hao Hou, March 2006										;

(setq :layer-Child "AIS-IB"
	  :layer-Label "AIS-Label"
	  :layer-Sequence "AIS-Sequence"
)

;;----------------------------------------------------------------------;
;;		Function:	ACTIVE-LAYER										;
;;----------------------------------------------------------------------;
(defun active-layer	()
  (vla-get-name (vla-get-activeLayer (active-document)))
)

;;----------------------------------------------------------------------;
;;		Function:	ADD-LAYER											;
;;----------------------------------------------------------------------;
(defun add-layer (name)
  (if (not (layer-p name))
	(vla-add (layer-all) name)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	LAYER-ALL											;
;;----------------------------------------------------------------------;
(defun layer-all () (vla-get-layers (active-document)))

;;----------------------------------------------------------------------;
;;		Function:	LAYER-BY-NAME										;
;;----------------------------------------------------------------------;
(defun layer-by-name (name)
  (if (layer-p name)
	(vlax-ename->vla-object (tblobjname "LAYER" name))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	LAYER-GETCOLOR										;
;;----------------------------------------------------------------------;
(defun layer-getColor (name)
  (vla-get-color (layer-by-name name))
)

;;----------------------------------------------------------------------;
;;		Function:	LAYER-P												;
;;----------------------------------------------------------------------;
(defun layer-p (var)
  (cond	((eq (type var) 'STR)
		 (if (tblsearch "LAYER" var)
		   t
		   nil
		 )
		)
		((and (vla-object-p var)
			  (= (vla-get-objectname var) "AcDbLayerTableRecord")
		 )
		 t
		 nil
		)
		(t nil)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	LAYER-SETACTIVE										;
;;----------------------------------------------------------------------;
(defun layer-setActive (name / layer)
  (if (= (type name) 'STR)
	(setq layer (layer-by-name name))
	(setq layer name)
  )
  (vla-put-activeLayer (active-document) layer)
)

;;----------------------------------------------------------------------;
;;		Function:	LAYER-SETCOLOR										;
;;----------------------------------------------------------------------;
(defun layer-setColor (name color)
  (vla-put-color (layer-by-name name) color)
)

;;----------------------------------------------------------------------;
;;		Function:	LAYER-LOCK											;
;;----------------------------------------------------------------------;
(defun layer-lock (name)
  (vla-put-lock (layer-by-name name) :vlax-true)
)

;;----------------------------------------------------------------------;
;;		Function:	LAYER-LOCKED?										;
;;----------------------------------------------------------------------;
(defun layer-locked? (name)
  (lsp-bool (vla-get-lock (layer-by-name name)))
)

;;----------------------------------------------------------------------;
;;		Function:	LAYER-UNLOCK										;
;;----------------------------------------------------------------------;
(defun layer-unlock	(name)
  (vla-put-lock (layer-by-name name) :vlax-false)
)

;; EOF