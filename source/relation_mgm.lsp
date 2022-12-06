;;		RELATION_MGM.LSP												;
;;		By June-Hao Hou, March 2006										;

;;//////////////////////////////////////////////////////////////////////;
;;		Constants														;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;	Relationships														;
;;----------------------------------------------------------------------;
(setq :rel-interpolate 'interpolate)
(setq :rel-derive 'derive)

;;----------------------------------------------------------------------;
;;	Properties															;
;;----------------------------------------------------------------------;
(setq :prop-ROG	"ROG"
	  :prop-ROB	"ROB"
	  :prop-ROG-disabled "ROGDisabled"
	  :prop-Observer "Observer"
	  :prop-Role "Role"
	  :prop-AisObjectName "AisObjectName"
	  :prop-ObjectData "ObjectData"
	  :prop-Sequence "Sequence"
	  :prop-MySeqParam "MySeqParam"
	  :prop-StartRef "StartRef"
	  :prop-EndRef "EndRef"
	  :prop-Children "Children"
	  :prop-ChildReactor "ChildReactor"
	  :prop-ChildProportion	"ChildProportion"
	  :prop-ChildInterpolation "ChildInterpolation"
	  :prop-NumSegments	"NumSegments"
	  :prop-Center "Center"
	  :prop-Radius "Radius"
	  :prop-StartAngle "StartAngle"
	  :prop-TotalAngle "TotalAngle"
	  :prop-TotalHeight	"TotalHeight"
)

(setq :role-Child "Child"
	  :role-Sequence "Sequence"
)

;;----------------------------------------------------------------------;
;;	Ruler of Generation													;
;;----------------------------------------------------------------------;
(setq :rog-interpolate 'interpolate
	  :rog-derive	   'derive
)

;;----------------------------------------------------------------------;
;;	Ruler of Binding													;
;;----------------------------------------------------------------------;
(setq :rob-Basepoint "Basepoint"
	  :rob-Bulge "Bulge"
	  :rob-Center "Center"
	  :rob-Elevation "Elevation"
	  :rob-EndAngle "EndAngle"
	  :rob-EndPoint	"EndPoint"
	  :rob-EndTangent "EndTangent"
	  :rob-Length "Length"
	  :rob-Midpoint "Midpoint"
	  :rob-Normal "Normal"
	  :rob-Radius "Radius"
	  :rob-StartAngle "StartAngle"
	  :rob-StartPoint "StartPoint"
	  :rob-StartTangent	"StartTangent"
	  :rob-Thickness "Thickness"
)

;;//////////////////////////////////////////////////////////////////////;
;;		Property Management												;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	CLEANUP-LDATA										;
;;																		;
;;	 Description:	Remove custom properties from all objects.			;
;;----------------------------------------------------------------------;
(defun cleanup-ldata (/ ss sslen index obj)
  (setq ss (ssget "_X"))
  (setq sslen (sslength ss))
  (setq index 0)
  (while (< index sslen)
	(foreach p (vlax-ldata-list
				 (setq obj (vlax-ename->vla-object (ssname ss index)))
			   )
	  (vlax-ldata-delete obj (car p))
	)
	(setq index (1+ index))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	GET-PROPERTY										;
;;----------------------------------------------------------------------;
(defun get-property (obj prop) (vlax-ldata-get obj prop))

;;----------------------------------------------------------------------;
;;		Function:	PUT-PROPERTY										;
;;----------------------------------------------------------------------;
(defun put-property	(obj prop val)
  (vlax-ldata-put obj prop val)
)

;;----------------------------------------------------------------------;
;;		Function:	DEL-PROPERTY										;
;;----------------------------------------------------------------------;
(defun del-property (obj prop) (vlax-ldata-delete obj prop))

;;----------------------------------------------------------------------;
;;		Function:	APPEND-PROPERTY										;
;;----------------------------------------------------------------------;
(defun append-property (obj prop val / lst)
  (if (setq lst (get-property obj prop))
	(put-property obj prop (append lst (list val)))
	(put-property obj prop (list val))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	DEL-PROPERTY										;
;;----------------------------------------------------------------------;


;;//////////////////////////////////////////////////////////////////////;
;;		Observer Management												;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;	Function:	ATTACH-OBSERVER											;
;;----------------------------------------------------------------------;
(defun attach-observer (subj observer / obs)
  (if (not (vl-consp observer))
	(setq observer (list observer))
  )
  (setq obs (get-property subj :prop-Observer))
  (foreach ob observer
	(if	(and (not (equal ob subj)) (not (member ob obs)))
	  (setq obs (append obs (list ob)))
	)
  )
  (put-property subj :prop-Observer obs)
)


;;----------------------------------------------------------------------;
;;	Function:	DETACH-OBSERVER											;
;;----------------------------------------------------------------------;
(defun detach-observer (subj observer / obs)
  (setq obs (get-property subj :prop-Observer))
  (if (member observer obs)
	(put-property subj :prop-Observer (vl-remove observer obs))
  )
)

;;----------------------------------------------------------------------;
;;	Function:	DEL-OBSERVER											;
;;----------------------------------------------------------------------;
(defun del-observer	(obj / oblst rogs)
  (setq oblst (get-property obj :prop-Observer))
  ;; 1. Remove related ROG from its observers.
  (foreach o oblst
	(setq rogs nil)
	(foreach rog (get-property ob :prop-ROG)
	  (cond	((and (eq (car rog) 'interpolate) (member obj rog))
			 (setq rogs (append rogs (list (subst nil obj rog))))
			)
			((and (eq (car rog) 'derive) (member obj rog)) nil)
			(T (setq rogs (append rogs (list rog))))
	  )
	)
	(put-property ob :prop-ROG rogs)
  )
  ;; 2. Remove its own observer list.
  (del-property obj :prop-Observer)
)

;;----------------------------------------------------------------------;
;;		Function:	HAS-OBSERVER?										;
;;----------------------------------------------------------------------;
(defun has-observer? (subj ob)
  (if (member ob (get-property subj :prop-Observer))
	T
	nil
  )
)

;;----------------------------------------------------------------------;
;;		Function:	DUMP-OBSERVER										;
;;----------------------------------------------------------------------;
(defun dump-observer (obj)
  (if (setq obs (get-property obj :prop-Observer))
	(progn (terpri)
		   (princ ";;----------------------")
		   (terpri)
		   (princ ";;Observer(s):")
		   (foreach	ob obs
			 (terpri)
			 (princ ";;    ")
			 (princ ob)
			 (if (vlax-erased-p ob)
			   (princ "-> ERASED")
			 )
		   )
		   (terpri)
	)
  )
)

;;//////////////////////////////////////////////////////////////////////;
;;		Rule of Generation (ROG)										;
;;																		;
;;		An AIS object may have the ROG for its regeneration. ROG is		;
;;		checked and evaluated before Rules of Binding (ROB).			;
;;		No multiple ROG for each object.								;
;;																		;
;;		Format:		(ROG-type . ROG-expression)							;
;;																		;
;;		Examples:	(:rog-interpolate obj1 obj2 0.5)					;
;;					(:rog-derive obj 0 3)								;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	ATTACH-ROG											;
;;----------------------------------------------------------------------;
(defun attach-rog (obj rog-type rog-list)
  (put-property obj :prop-ROG (cons rog-type rog-list))
)

;;----------------------------------------------------------------------;
;;		Function:	DEL-ROG												;
;;----------------------------------------------------------------------;
(defun del-rog (obj / subj-list)
  ;; Collect associated subjets
  (foreach a (cdr (get-property obj :prop-ROG))
	(if	(vla-object-p a)
	  (setq subj-list (cons a subj-list))
	)
  )
  ;; Remove obj from subjects' observer list
  (foreach subj subj-list (detach-observer subj obj))
  ;; Safely remove ROG property
  (vlax-ldata-delete obj :prop-ROG)
)

;;----------------------------------------------------------------------;
;;		Function:	MOD-ROG-INTERPOLATE									;
;;----------------------------------------------------------------------;
(defun mod-rog-interpolate (obj parent1 parent2 param)
  (setq rog (get-property obj :prop-ROG))
  (if (null parent1)
	(setq parent1 (cadr rog))
  )
  (if (null parent2)
	(setq parent2 (caddr rog))
  )
  (if (null param)
	(setq param (last rog))
  )
  (if (and (object-identical-p parent1 parent2)
		   (object-identical-p obj parent1)
	  )
	(put-property
	  obj
	  :prop-ROG
	  (list :rog-interpolate parent1 parent2 param)
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	MOD-ROG-DERIVE										;
;;----------------------------------------------------------------------;
(defun mod-rog-derive (obj parent from to)
  (setq rog (get-property obj :prop-ROG))
  (if (null parent)
	(setq parent (cadr rog))
  )
  (if (null from)
	(setq from (caddr rog))
  )
  (if (null to)
	(setq to (+ from (- (last rog) (caddr rog))))
  )
  (put-property
	obj
	:prop-ROG
	(list :rog-derive parent from to)
  )
)

;;----------------------------------------------------------------------;
;;		Function: DUMP-ROG												;
;;----------------------------------------------------------------------;
(defun dump-rog	(obj)
  (if (setq rog (get-property obj :prop-ROG))
	(progn (terpri)
		   (princ ";;----------------------")
		   (terpri)
		   (princ ";;Rule of Generation:")
		   (cond ((eq (car rog) :rog-interpolate)
				  (princ " INTERPOLATE")
				  (terpri)
				  (princ ";;  Between ")
				  (princ (cadr rog))
				  (if (vlax-erased-p (cadr rog))
					(princ " -> ERASED")
				  )
				  (terpri)
				  (princ ";;      and ")
				  (princ (caddr rog))
				  (if (vlax-erased-p (caddr rog))
					(princ " -> ERASED")
				  )
				  (terpri)
				  (princ ";;  Parameter = ")
				  (princ (last rog))
				 )
				 ((eq (car rog) :rog-derive)
				  (princ " DERIVE")
				  (terpri)
				  (princ ";;  From ")
				  (princ (cadr rog))
				  (if (vlax-erased-p (cadr rog))
					(princ " -> ERASED")
				  )
				  (terpri)
				  (princ ";;  Parameter ")
				  (princ (caddr rog))
				  (princ " to ")
				  (princ (last rog))
				 )
		   )
		   (terpri)
	)
  )
)

;;//////////////////////////////////////////////////////////////////////;
;;		Rule of Binding	(ROB)											;
;;																		;
;;		An AIS object may have zero or more ROBs for property binding.	;
;;		Each ROB associates one of the object's properties to an		;
;;		expression which returns a proper data type for the property.	;
;;																		;
;;		Format:		((ROB-property . ROB-expression) ...)				;
;;																		;
;;		Examples:	(("StartPoint" 'ais:endpoint object1)				;
;;					 ("EndPoint" 'ais:midpoint object2))				;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	ATTACH-ROB											;
;;																		;
;;		Usage: (attach-rob VLA-object "StartPoint"						;
;;					(list 'curve-get-point-at-ppt VLA-object 0.5))		;
;;----------------------------------------------------------------------;
(defun attach-rob (obj rob-type rob-list / robs)
  (detach-rob obj rob-type)
  (if (setq robs (get-property obj :prop-ROB))
	;; Detach existing rob-type first before append to the ROB property.
	(progn (put-property
			 obj
			 :prop-ROB
			 (append robs (list (cons rob-type rob-list)))
		   )
	)
	;; Create new ROB property
	(put-property obj :prop-ROB (list (cons rob-type rob-list)))
  )
  ;; Add object to the observer list of each subject.
  ;|(foreach a rob-list
	(if	(vla-object-p a)
	  (progn (attach-reactor 'notifier a) (attach-observer a obj))
	)
  )|;
)


;;----------------------------------------------------------------------;
;;		function:	BIND												;
;;																		;
;;	Description:	Alias of ATTACH-ROB									;
;;																		;
;;		Usage: (bind VLA-object "StartPoint"							;
;;					(list 'curve-get-point-at-ppt VLA-object 0.5) t)	;
;;----------------------------------------------------------------------;
(defun bind	(obj rob rob-list rflag)
  (attach-rob obj rob rob-list)
  (if rflag
	(foreach a rob-list
	  (if (vla-object-p a)
		(progn (attach-reactor 'notifier a) (attach-observer a obj))
	  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	DETACH-ROB											;
;;																		;
;;		Usage: (detach-rob VLA-object "StartPoint")						;
;;----------------------------------------------------------------------;
(defun detach-rob (obj rob-type / robs)
  (if (setq robs (get-property obj :prop-ROB))
	(put-property
	  obj
	  :prop-ROB
	  (vl-remove (assoc rob-type robs) robs)
	)
  )
)

;;----------------------------------------------------------------------;
;;		function:	UNBIND												;
;;																		;
;;	Description:	Alias of DETACH-ROB									;
;;																		;
;;		Usage: (unbind VLA-object "StartPoint")							;
;;----------------------------------------------------------------------;
(defun unbind (obj rob-type) (detach-rob obj rob-type))

;;----------------------------------------------------------------------;
;;		Function:	PURGE-ORPHEN-ROB									;
;;																		;
;;	 Description:	Validate ROBs of an object and remove those who		;
;;					associate with erased subjects.						;
;;----------------------------------------------------------------------;
(defun purge-orphen-rob	(obj / robs robs-new oops)
  (if (setq robs (get-property obj :prop-ROB))
	(progn (foreach	rob	robs
			 (setq oops nil)
			 (if (vl-consp (cdr rob))
			   (foreach	a (cdr rob)
				 (if (and (vla-object-p a) (vlax-erased-p a))
				   (setq oops t)
				 )
			   )
			   (if (and (vla-object-p (cdr rob)) (vlax-erased-p (cdr rob)))
				 (setq oops t)
			   )
			 )
			 (if (not oops)
			   (setq robs-new (append robs-new (list rob)))
			 )
		   )
		   (put-property obj :prop-ROB robs-new)
	)
	nil
  )
)


;;----------------------------------------------------------------------;
;;		Function:	DEL-ROB												;
;;																		;
;;		Description: Remove the binding relation from an object.		;
;;----------------------------------------------------------------------;
(defun del-rob (obj) (vlax-ldata-delete obj :prop-ROB))

;;----------------------------------------------------------------------;
;;		Function:	DUMP-ROB											;
;;----------------------------------------------------------------------;
(defun dump-rob	(obj / n)
  (if (setq robs (get-property obj :prop-ROB))
	(progn
	  (terpri)
	  (princ ";;----------------------")
	  (terpri)
	  (princ ";;Rule(s) of Binding:")
	  (setq n 1)
	  (foreach rob robs		  ; Step thru ROBs
		(terpri)
		(princ (strcat ";;  [" (itoa n) "] "))
		(princ (strcase (car rob)))
		(princ " =")
		(if	(vl-consp (cdr rob))
		  (foreach a (cdr rob)
			(princ " ")
			(cond ((vla-object-p a)
				   (princ a)
				   (if (vlax-erased-p a)
					 (princ "-!ERASED!")
				   )
				  )
				  (T (princ a))
			)
		  )
		  (progn (princ " ")
				 (princ (cdr rob))
				 (if (and (vla-object-p (cdr rob)) (vlax-erased-p (cdr rob)))
				   (princ "-!ERASED!")
				 )
		  )
		)
		(setq n (1+ n))
	  )
	  (terpri)
	)
  )
)








;; EOF
