;;	REACTOR.LSP															;
;;	By June-Hao Hou, March 2006											;
(vl-load-com)


;;----------------------------------------------------------------------;
;;		Function:	AIS-PERS											;
;;----------------------------------------------------------------------;
(defun aisr-pers () (foreach r (ais-reactors) (vlr-pers r)))

;;----------------------------------------------------------------------;
;;		Function:	AIS-REACTORS										;
;;----------------------------------------------------------------------;
(defun ais-reactors	()
  (cdar (vlr-reactors :vlr-object-reactor))
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-REACTOR-SETUP									;
;;																		;
;;	Description:	When open a DWG with persistent reactors, check to	;
;;					see if there is a Notifier. If yes, then use it.	;
;;					This function is involked by AIS-INIT procedure.	;
;;----------------------------------------------------------------------;
(defun ais-reactor-setup ()
  ;; Restore *aisr-notifier* if the notifier reactor exists in the file.
  (if (not *aisr-notifier*)
	(foreach r (ais-reactors)
	  (if (eq (vlr-data r) "AIS-Notifier")
		(setq *aisr-notifier* r)
	  )
	)
  )
  ;; DWG reactor to make all reactors persistent before saving file.
  (if (not (vlr-reactors :vlr-dwg-reactor))
	(vlr-dwg-reactor
	  "AIS"
	  (list (cons :vlr-beginsave (function cb-dwg-beginsave)))
	)
  )
)

		   
;;----------------------------------------------------------------------;
;;		Function:	ATTACH-REACTOR										;
;;																		;
;;		Arguments:														;
;;			symbol	sym		Custom type of reactor.						;
;;			vlaobj	obj		VLA object to attach reactor.				;
;;																		;
;;		Requirements:													;
;;					add-notifier-reactor								;
;;																		;
;;		Usage:	(attach-reactor 'notifier VLA-object)					;
;;----------------------------------------------------------------------;
(defun attach-reactor (sym obj / seq r)
  (if (not (vl-consp obj))
	(setq obj (list obj))
  )
  (cond	;; Notifier reactor
		((eq sym 'notifier)
		 (if *aisr-notifier*
		   (foreach o obj (vlr-owner-add *aisr-notifier* o))
		   (setq *aisr-notifier* (add-notifier-reactor obj))
		 )
		 *aisr-notifier*
		)
		;; Child reactor
		((eq sym 'child)
		 (setq seq (get-property (car obj) :prop-Sequence))
		 (if (setq r (get-property seq :prop-ChildReactor))
		   (foreach o obj (vlr-owner-add r o))
		   (put-property
			 seq
			 :prop-ChildReactor
			 (setq r (add-child-reactor obj seq))
		   )
		 )
		 r
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	DISABLE-REACTOR										;
;;																		;
;;		Usage:															;
;;			(disable-reactorr 'notifier)								;
;;----------------------------------------------------------------------;
(defun disable-reactor (sym)
  (cond	((eq sym 'notifier) (vlr-remove *aisr-notifier*))
		((eq sym 'child) (vlr-remove *aisr-child*))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ENABLE-REACTOR										;
;;																		;
;;		Usage: (enable-reactorr 'notifier)								;
;;----------------------------------------------------------------------;
(defun enable-reactor (sym)
  (cond	((eq sym 'notifier) (vlr-add *aisr-notifier*))
		((eq sym 'child) (vlr-add *aisr-child*))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ADD-NOTIFIER-REACTOR								;
;;																		;
;;		Usage: (add-notifier-reactor (list obj1 obj2))					;
;;----------------------------------------------------------------------;
(defun add-notifier-reactor	(owners)
  (vlr-object-reactor
	owners
	"AIS-Notifier"
	(list (cons :vlr-modified (function cb-object-modified))
	  (cons :vlr-objectClosed (function cb-object-closed))
	  (cons :vlr-erased (function cb-object-erased))
	 )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ADD-CHILD-REACTOR									;
;;----------------------------------------------------------------------;
(defun add-child-reactor (owners seq)
  (vlr-object-reactor
	owners
	seq
	(list 
	  ;;(cons :vlr-modified (function cb-child-modified))
	  ;;(cons :vlr-copied (function cb-child-copied))
	  (cons :vlr-erased (function cb-child-erased))
	 )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CB-OBJECT-MODIFIED									;
;;																		;
;;		Requirements:													;
;;					get-property										;
;;----------------------------------------------------------------------;
(defun cb-object-modified (notifier reactor data)
  (if *ais-debug*
	(setq *ais-modified-calls* (1+ *ais-modified-calls*))
  )
  (if (vlr-added-p reactor)
	(progn (setq *ais-objectsToModify* nil)
		   (collect-observers notifier)
	)
  )
  ;|(foreach ob (get-property notifier :prop-Observer)
	;; Prevent from repeating update
	(if	(and (not (member ob *ais-objectsToModify*))
			 (not (vlax-erased-p ob))
		)
	  (setq *ais-objectsToModify* (append *ais-objectsToModify* (list ob)))
	)
  )|;
)


;;----------------------------------------------------------------------;
;;		Function:	CB-OBJECT-CLOSED									;
;;																		;
;;		Requirements:													;
;;					ais-update											;
;;----------------------------------------------------------------------;
(defun cb-object-closed	(notifier reactor data)
  (if *ais-objectsToModify*
	(progn (setq *ais-objectsModifying* (append *ais-objectsToModify* (list (list reactor))))
		   (setq *ais-objectsToModify* nil)
		   (if *ais-debug*
			 (progn	(terpri)
					(princ "Propagate change to ")
					(princ (length *ais-objectsModifying*))
					(princ " objects.")
					(terpri)
			 )
		   )
		   (vlr-remove reactor) ; Disable reactor
		   (if (and *ais-debug* (not (vlr-added-p reactor)))
			 (princ "*reactor off*")
		   )
		   (foreach obj *ais-objectsModifying* (ais-update obj))
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CB-OBJECT-ERASED									;
;;																		;
;;		Requirements:													;
;;					?													;
;;----------------------------------------------------------------------;
(defun cb-object-erased (notifier reactor data)
  ;; if obj is a sequence path or start/end-ref, then dismiss the sequence.
  nil)

;;----------------------------------------------------------------------;
;;		Function:	COLLECT-OBSERVERS									;
;;----------------------------------------------------------------------;
(defun collect-observers (obj / temp)
  (if (not (vl-consp obj))
	(setq obj (list obj))
  )
  (foreach o obj
	(if	(setq observers (get-property o :prop-Observer))
	  (foreach ob observers
		(if	(and (not (member ob *ais-objectsToModify*))
				 (not (member ob temp)))
		  (setq temp (append temp (list ob)))
		)
	  )
	)
  )
  (if temp
	(progn (setq *ais-objectsToModify* (append *ais-objectsToModify* temp))
		   (collect-observers temp)
	)
  )
)

;; This search function is depth-first, which does not work!
;; The order of update is VERY IMPORTANT!
(defun collect-observers*** (obj)
  (if (setq observers (get-property obj :prop-Observer))
	(foreach ob	observers
	  (if (not (member ob *ais-objectsToModify*))
		(progn (setq *ais-objectsToModify* (append *ais-objectsToModify* (list ob)))
			   (collect-observers ob)
		)
	  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CB-OBJECT-MODIFIED									;
;;----------------------------------------------------------------------;
(defun cb-child-modified (notifier reactor data)
  nil)

;;----------------------------------------------------------------------;
;;		Function:	CB-CHILD-COPIED										;
;;																		;
;;	Description:	!!! WEIRD !!!										;
;;					When dragging polyline this function				;
;;					is triggered!										;
;;----------------------------------------------------------------------;
(defun cb-child-copied (notifier reactor data)
  (if (and data (vl-consp data) (vla-object-p (car data)))
	(foreach newborn data
	  (put-property
		newborn
		:prop-ROG
		(get-property notifier :prop-ROG)
	  )
	  (put-property
		newborn
		:prop-ROB
		(get-property notifier :prop-ROB)
	  )
	  (seq-add-child
		(get-property notifier :prop-Sequence)
		newborn
		(+ (get-property notifier :prop-MySeqParam) 0.001)
	  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	CB-CHILD-ERASED										;
;;----------------------------------------------------------------------;
(defun cb-child-erased (notifier reactor data)
  ;; remove me from the sequence's child list
  (seq-del-child (vlr-data reactor) notifier)
  (vlr-owner-remove reactor notifier)
)


;;----------------------------------------------------------------------;
;;		Function:	CB-DWG-BEGINSAVE									;
;;																		;
;;	Description:	Before saving to DWG, making all object reactors	;
;;					persistent.											;
;;----------------------------------------------------------------------;
(defun cb-dwg-beginsave (reactor data)
  (aisr-pers)
)

;; EOF