;;		IO_COMMAND.LSP													;
;;		By June-Hao Hou, March 2006										;
(vl-load-com)

;;//////////////////////////////////////////////////////////////////////;
;;		SysVars control													;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	COMMAND-BEGIN										;
;;----------------------------------------------------------------------;
(defun command-begin ()
  (if (not save-cmdecho)
	(setq save-cmdecho (getvar "CMDECHO"))
  )
  (if (not save-blipmode)
	(setq save-blipmode (getvar "BLIPMODE"))
  )
  (if (not save-osmode)
	(setq save-osmode (getvar "OSMODE"))
  )
  (if (not save-autosnap)
	(setq save-autosnap (getvar "AUTOSNAP"))
  )
  (if (not save-plinetype)
	(setq save-plinetype (getvar "PLINETYPE"))
  )
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setvar "OSMODE" 0)
  (setvar "AUTOSNAP" 0)
  (setvar "PLINETYPE" 2)
)

;;----------------------------------------------------------------------;
;;		Function:	COMMNAD-END											;
;;----------------------------------------------------------------------;
(defun command-end ()
  (setvar "CMDECHO" save-cmdecho)
  (setvar "BLIPMODE" save-blipmode)
  ;;(setvar "OSMODE" save-osmode)
  ;;(setvar "AUTOSNAP" save-autosnap)
  (setvar "PLINETYPE" save-plinetype)
  (setq	save-cmdecho nil
		save-blipmode nil
		save-osmode	nil
		save-autosnap nil
		save-plinetype nil
  )
  (terpri)
)


;;//////////////////////////////////////////////////////////////////////;
;;		Buttonbar commands												;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	C:BINDDEF											;
;;----------------------------------------------------------------------;
(defun c:binddef ()
  (command-begin)
  (if *ais-initialized*
	;;(bind-cmd)
	(alert "Not fully implemented yet. Sorry.")
	(error-ais-not-initialized)
  )
  (command-end)
)

;;----------------------------------------------------------------------;
;;		Function:	C:BINDEDIT											;
;;----------------------------------------------------------------------;
(defun c:addobsubjrel ()
  (command-begin)
  (if *ais-initialized*
	(add-ob-subj-relation)
	(error-ais-not-initialized))
  (command-end)
)

;;----------------------------------------------------------------------;
;;		Function:	C:SEQDEF											;
;;----------------------------------------------------------------------;
(defun c:seqdef	()
  (command-begin)
  (if *ais-initialized*
	(seq-def)
	(error-ais-not-initialized)
  )
  (command-end)
)

;;----------------------------------------------------------------------;
;;		Function:	C:SEQEDIT											;
;;----------------------------------------------------------------------;
(defun c:seqedit ()
  (terpri)
  (princ "Placeholder for Edit Sequence button.")
  (princ)
)

;;----------------------------------------------------------------------;
;;		Function:	C:MKKEY												;
;;----------------------------------------------------------------------;
(defun c:mkkey ()
  (command-begin)
  (terpri)
  (princ "Switch Key On/Off --> ")
  (setq obj (objsel))
  (if (and obj
		   (vla-object-p obj)
		   (eq (get-property obj :prop-Role) "Child")
		   (setq seq (get-property obj :prop-Sequence))
		   (member obj (seq-get-children seq))
	  )
	(if	(member obj (get-property seq :prop-Children))
	  (make-child-idol seq obj)
	  (make-child-mundane seq obj)
	)
  )
  (command-end)
)

;;----------------------------------------------------------------------;
;;		Function:	C:DEKEY												;
;;----------------------------------------------------------------------;
(defun c:dekey ()
  (command-begin)
  (terpri)
  (setq obj (objsel))
  (if (and (eq (get-property obj :prop-Role) "Child")
		   (setq seq (get-property obj :prop-Sequence))
	  )
	(make-child-mundane seq obj)
  )
  (command-end)
)

;;----------------------------------------------------------------------;
;;		Function:	C:DEKEY												;
;;----------------------------------------------------------------------;
(defun c:addchild ()
  (command-begin)
  (terpri)
  (princ "Sequence --> ")
  (setq seq (objsel))
  (terpri)
  (princ "Child --> ")
  (setq child (objsel))
  (if (and (sequence-p seq) (vla-object-p child))
	(progn (terpri)
		   (princ "Pick the insertion point on the sequence path...")
		   (terpri)
		   (if (setq ppt (car (pick-n-value-on-curve seq 1 0)))
			 (seq-add-child seq child ppt)
		   )
	)
  )
  (command-end)
)

;;----------------------------------------------------------------------;
;;		Function:	C:AISUPDATE											;
;;----------------------------------------------------------------------;
(defun c:aisupdate () (ais-touch (objsel)))

;;----------------------------------------------------------------------;
;;		Function:	C:AISGETINFO										;
;;----------------------------------------------------------------------;
(defun c:aisgetinfo	(/ continue obj)
  (command-begin)
  (setq continue t)
  (if *ais-initialized*
	(while continue
	  (terpri)
	  (initget "eXit")
	  (setq obj (entsel "Select object [eXit]: "))
	  (cond	((eq "eXit" obj) (setq continue nil))
			((and (vl-consp obj) (eq 'ENAME (type (car obj))))
			 (dump-relation (vlax-ename->vla-object (car obj)))
			)
			(t (setq continue nil))
	  )
	)
	(error-ais-not-initialized)
  )
  (command-end)
)

;;----------------------------------------------------------------------;
;;		Function:	AIS:AISINTERPOLATE									;
;;----------------------------------------------------------------------;
(defun c:aisinterpolate	()
  (terpri)
  (ais-interpolate (objsel) (objsel) (getreal "Proportion: "))
  (princ)
)

;;----------------------------------------------------------------------;
;;		Function:	C:AISDERIVE											;
;;----------------------------------------------------------------------;
(defun c:aisderive ()
  (terpri)
  (princ "Placeholder for derive button.")
  (princ)
)

;;----------------------------------------------------------------------;
;;		Function:	BIND-CMD											;
;;----------------------------------------------------------------------;
(defun bind-cmd	(/)
  (terpri)
  (princ "The Binding Object:")
  (terpri)
  (setq obj-in (objsel))
  (setq inlets (available-inlets obj-in))
  (terpri)
  (apply 'initget (list (str-implode " " inlets)))
  (setq	prop-in	(getint
				  (strcat "Select property [" (str-implode "/" inlets) "]: ")
				)
  )
  ;; determine corresponding outlet type
  (setq datatype (cdr (assoc prop-in *ais-property-type*)))
  (terpri)
  (princ "The Bound Object:")
  (terpri)
  (setq obj-out (objsel))
  ;; select proper outlets based on the data type
  (setq outlets (available-outlets obj-out datatype))
  (terpri)
  (apply 'initget
		 (list
		   (str-implode " " (append outlets (list " eXpression")))
		 )
  )
  (setq	prop-out (getint (strcat "Select property ["
								 (str-implode "/" outlets)
								 "/eXpression]: "
						 )
				 )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	DUMP-RELATION										;
;;----------------------------------------------------------------------;
(defun dump-relation (obj)
  (terpri)
  (princ
	";;___________________________________________________________"
  )
  (terpri)
  (princ ";;VLA Object ID: ")
  (princ obj)
  (dump-ais-object-type obj)
  (dump-ais-role obj)
  (dump-observer obj)
  (dump-rog obj)
  (dump-rob obj)
  (terpri)
  (princ ";;----------------------")
  (terpri)
  (princ ";;END OF LIST")
  (terpri)
)

;;----------------------------------------------------------------------;
;;		Function:	DUMP-AIS-OBJECT-TYPE								;
;;----------------------------------------------------------------------;
(defun dump-ais-object-type	(obj / objtype data)
  (if (and obj
		   (vla-object-p obj)
		   (setq objtype (get-property obj :prop-AisObjectName))
	  )
	(progn (terpri)
		   (princ ";;AIS Object Type: ")
		   (cond ((eq (strcase objtype) "SPIRAL")
				  (setq	data (list :prop-Center			  :prop-Radius
								   :prop-StartAngle		  :prop-TotalAngle
								   :prop-TotalHeight	  :prop-NumSegments
								  )
				  )
				 )
		   )
		   (foreach	d data
			 (terpri)
			 (princ ";;  ")
			 (princ d)
			 (princ " = ")
			 (princ (get-property obj d))
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	DUMP-AIS-ROLE										;
;;----------------------------------------------------------------------;
(defun dump-ais-role (obj / objrole data)
  (if (and obj
		   (vla-object-p obj)
		   (setq objrole (get-property obj :prop-Role))
	  )
	(progn (terpri)
		   (princ ";;AIS Role: ")
		   (princ (strcase objrole))
		   (cond ((eq (strcase objrole) "SEQUENCE")
				  (setq	data (list :prop-StartRef			 :prop-EndRef
								   :prop-NumSegments		 :prop-Children
								   :prop-ChildProportion	 :prop-ChildInterpolation
								   :prop-ChildReactor
								  )
				  )
				 )
				 ((eq (strcase objrole) "CHILD")
				  (setq data (list :prop-Sequence :prop-MyseqParam))
				 )
		   )
		   (foreach	d data
			 (terpri)
			 (princ ";;  ")
			 (princ d)
			 (princ " = ")
			 (princ (get-property obj d))
		   )
	)
  )
)

;;//////////////////////////////////////////////////////////////////////;
;;		PEDIT2 Command and Functions									;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	C:PEDIT2											;
;;----------------------------------------------------------------------;
(defun c:pedit2	(/ obj continue num)
  (setq obj (vlax-ename->vla-object (car (entsel "Select polyline: "))))
  (terpri)
  (if (pline-p obj)
	(progn (setq continue t)
		   (while continue
			 (pline-pointnumber-show obj)
			 (initget 0 "Reverse Basepoint Edit")
			 (setq num (getint
						 "Enter an option [Reverse/Basepoint/Edit vertex/<point#>]: "
					   )
			 )
			 (cond ((eq num "Reverse") (pline-reverse obj))
				   ((eq num "Basepoint") (pedit2-basepoint obj))
				   ((eq num "Edit") (pedit2-ptedit obj 0))
				   ((numberp num)
					(if	(and (>= num 0) (< num (pline-numberOfPoints obj)))
					  (pedit2-ptedit obj num)
					)
				   )
				   (t (setq continue nil))
			 )
		   )
		   (pline-pointnumber-hide obj)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PEDIT2-BASEPOINT									;
;;----------------------------------------------------------------------;
(defun pedit2-basepoint	(obj / numPoints num)
  (setq numPoints (pline-numberOfPoints obj))
  (initget 4)
  (setq	num	(getint	(strcat	"Assign new basepoint (0-"
							(itoa (1- numPoints))
							"): "
					)
			)
  )
  (if (and (>= num 0) (< num numPoints))
	(pline-setBasepoint obj num)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	PEDIT2-PTEDIT										;
;;																		;
;;	Description:	PEDIT2 sub-function for point editing.				;
;;----------------------------------------------------------------------;
(defun pedit2-ptedit (obj idx / continue index numPoints num pt bg)
  (setq continue t)
  (setq index idx)
  (setq numPoints (pline-numberOfPoints obj))
  (crosshair-show)
  (while continue
	(crosshair-move (pline-getProp 'point obj index))
	(initget 0 "Next Previous First Last Insert Del eXit")
	(setq num (getreal
				"Edit vertex [Next/Previous/First/Last/Insert/Del/eXit/<bulge>] :"
			  )
	)
	(cond ;; Next point
		  ((eq num "Next")
		   (if (>= (1+ index) numPoints)
			 (setq index 0)
			 (setq index (1+ index))
		   )
		  )
		  ;; Previous point
		  ((eq num "Previous")
		   (if (< (1- index) 0)
			 (setq index (1- numPoints))
			 (setq index (1- index))
		   )
		  )
		  ;; Jump to the first point
		  ((eq num "First") (setq index 0))
		  ;; Jump to the last point
		  ((eq num "Last") (setq index (1- numPoints)))
		  ;; Insert after current point
		  ((eq num "Insert")
		   (terpri)
		   (setq pt	(getpoint (pline-getProp 'point obj index)
							  "  Enter point: "
					)
		   )
		   (terpri)
		   (if pt
			 (progn	(pline-addVertex obj (1+ index) pt)
					(setq numPoints (pline-numberOfPoints obj))
					(pline-pointnumber-show obj)
			 )
		   )
		  )
		  ;; Delete current point
		  ((eq num "Del")
		   (setq pt	(chunk (pline-getPropAll 'point obj) 2)
				 bg	(pline-getPropAll 'bulge obj)
		   )
		   (setq pt	(append (left index pt) (right (- numPoints index 1) pt))
				 bg	(append (left index bg) (right (- numPoints index 1) bg))
		   )
		   (pline-entmod obj pt bg nil nil nil nil)
		   (setq numPoints (pline-numberOfPoints obj))
		   (if (>= index numPoints)
			 (setq index (1- index))
		   )
		   (pline-pointnumber-show obj)
		  )
		  ;; Exit
		  ((eq num "eXit") (setq continue nil)) ; Exit
		  ((numberp num) (pline-setProp 'bulge obj index num))
		  ;; Otherwise, exit.
		  (T (setq continue nil))
	)
  )
  (crosshair-hide)
)


;;//////////////////////////////////////////////////////////////////////;
;;		OBJSEL Command and Functions									;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	C:OBJSEL											;
;;----------------------------------------------------------------------;
(defun c:objsel	(/ obj)
  ;;(terpri)
  (initget 0 "Interpolate Derive")
  (setq obj (entsel "Select object [Interpolate/Derive]: "))
  (cond	((eq obj "Interpolate") (objsel-interpolate))
		((eq obj "Derive") (objsel-derive))
		((vl-consp obj) (vlax-ename->vla-object (car obj)))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	OBJSEL-INTERPOLATE									;
;;----------------------------------------------------------------------;
(defun objsel-interpolate (/ obj1 obj2 num)
  (terpri)
  (princ "By interpolation...")
  (terpri)
  (setq obj1 (entsel "    Select first object: "))
  (if (vl-consp obj1)
	(progn (setq obj1 (vlax-ename->vla-object (car obj1)))
		   (terpri)
		   (setq obj2 (entsel "    Select second object: "))
		   (if (vl-consp obj2)
			 (progn	(setq obj2 (vlax-ename->vla-object (car obj2)))
					(terpri)
					(initget 1)
					(setq num (getreal "    Enter proportion (0..1): "))
					(if	(and (numberp num) (>= num 0.0) (<= num 1.0))
					  (ais-interpolate obj1 obj2 num)
					)
			 )
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	OBJSEL-DERIVE										;
;;----------------------------------------------------------------------;
(defun objsel-derive (/ obj from to maxPt)
  (terpri)
  (princ "By derivation...")
  (terpri)
  (setq obj (entsel "   Select source object: "))
  (if (vl-consp obj)
	(progn (setq obj (vlax-ename->vla-object (car obj)))
		   (cond ((pline-p obj) (objsel-derive-pline obj))
				 ((circle-p obj) (objsel-derive-circle obj))
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	OBJSEL-DERIVE-PLINE									;
;;----------------------------------------------------------------------;
(defun objsel-derive-pline (obj / maxPt from to)
  (setq maxPt (1- (pline-numberOfPoints obj)))
  (pline-pointnumber-show obj)
  (terpri)
  (initget 0)
  (setq from (getint (strcat "    Start point# (0-" (itoa maxPt) "): ")))
  (terpri)
  (initget 0)
  (setq to (getint (strcat "    End point# (0-" (itoa maxPt) "): ")))
  (pline-pointnumber-hide obj)
  (if (and from to (>= (min from to) 0) (<= (max from to) maxPt))
	(pline-derive obj from to)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	OBJSEL-DERIVE-CIRCLE								;
;;----------------------------------------------------------------------;
(defun objsel-derive-circle ()
  nil)


;;//////////////////////////////////////////////////////////////////////;
;;		Object Binding Commands and Functions							;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	BIND-DEF											;
;;----------------------------------------------------------------------;
(defun add-ob-subj-relation	(/ ob subj)
  (terpri)
  (princ "Observer --> ")
  (setq ob (objsel))
  (terpri)
  (princ "Subject --> ")
  (setq subj (objsel))
  (if (and (vla-object-p ob) (vla-object-p subj))
	(progn (attach-observer subj ob)
		   (attach-reactor 'notifier subj)
	)
  )
  ;; select first object (the binding object) and highlight it
  ;; select which property to bind (inlets)
  ;; determine data type
  ;; select second object (the bound object) and highlight it
  ;; select outlets by data type
  ;; determine ROB data
  ;; save ROB data
)


;;----------------------------------------------------------------------;
;;		Function:	BIND-EDIT											;
;;----------------------------------------------------------------------;
(defun bind-edit () nil)


;;//////////////////////////////////////////////////////////////////////;
;;		SEQ Command and Functions										;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	C:SEQ												;
;;----------------------------------------------------------------------;
(defun seq-def (/ path start end num)
  (terpri)
  (princ "*** Define Sequence ***")
  (terpri)
  (princ "Path --> ")
  (if (setq path (c:objsel))
	(progn (terpri)
		   (princ "Start --> ")
		   (if (setq start (c:objsel))
			 (progn	(terpri)
					(princ "End --> ")
					(setq end (c:objsel))
					(terpri)
					(initget 6) ; Prevent zero and negative number
					(setq num (getint "Number of segments <1>: "))
					(if	(not num)
					  (setq num 1)
					)
					(seq-psen path start end num)
			 )
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-EDIT											;
;;----------------------------------------------------------------------;
(defun seq-edit	()
  ;; select a path object
  ;; varify if it's a sequence
  ;; read in sequence properties
  ;; show information and wait for command
  ;; (select second obj) (set num of segments/intervals)
  nil
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-STARTUP											;
;;----------------------------------------------------------------------;
(defun-q AIS-STARTUP () (c:ais-init))

(setq S::STARTUP (append S::STARTUP AIS-STARTUP))

;;	Initializa AIS system
(princ "AIS2 is loaded.")
(terpri)
(princ)
;; EOF