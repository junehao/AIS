;;	SEQUENCE.LSP														;
;;	By June-Hao Hou, March 2006											;

;|	[Synopsis]
	--
	A sequence always has a master reference line (MasterRef or Path), which can be any
	curve object. The master reference line is the graphic
	representation of a sequence object. In addition to the geometric feature,
	it also carries specific properties for the sequence.
	--
	Compare to an animation sequence, the MasterRef is the timeline with
	all related properties such as start timecode, end timecode,
	number of frames, references to included tracks (objects) and so on.
	--
	To make a new sequence, one needs to pick a master reference
	first--and that is the only required process. A few specific properties
	are added to the object so it can be recognized as an AIS object.
	The process of making a path object a sequence is called "blessing".
	After it is blessed, the object becomes a sequence.
	--
	Examples of making a sequence:
	(setq path-object (objsel))
	(setq railing-proto (sequence path-object))
	; default properties are: numIntervals=1, startEnt=nil, endEnt=nil,
	;	startParam=0.5, endParam=0.5, startAng=0, endAng=0, children=nil
	(sequence-setProp railing-proto '((numIntervals . 8) (...)...))
|;
;;		What is a family?												;
;;																		;
;;		A family is a list of objects in a sequence, providing a easy	;
;;		way to access all family members, i.e. objects in a sequence.	;
;;																		;
;;		A family list has the format of:								;
;;			(P1 C1 C2 C3 C4 ... Cn P2)									;
;;		Where P1 & P2 are Parents, and C1...Cn are children.			;
;;																		;
;;		Usually chidren are cloned or produced by interpolation of		;
;;		the parents. P1 and P2 have parameters 0 and 1 on the path.		;
;;		Each children's param is thus evenly divided between 0 & 1.		;
;;																		;
;;		We can make any number of the children idols, others remain		;
;;		as mundane. The roles of Parents and Idol childs are keyframes	;
;;		in an animation timeline. Parents hold the start and end points,;
;;		while al children sit between.									;

;;----------------------------------------------------------------------;
;;		Function:	SEQUENCE-P											;
;;----------------------------------------------------------------------;
(defun sequence-p (obj)
  (and (vla-object-p obj)
	   (eq (get-property obj :prop-Role) :role-Sequence)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-SEN												;
;;																		;
;;	Description:	Making a new sequence by giving 3 arguments:		;
;;					Start-End-Number.									;
;;----------------------------------------------------------------------;
(defun seq-sen (start end num)
  (setq path (vla-add3dpoly (model-space) (LtoV '(0 0 0 1 0 0))))
  (vla-put-coordinate path 0 (ais:basepoint start))
  (vla-put-coordinate path 1 (ais:basepoint end))
  (seq-psen path start end num)
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-PSN												;
;;																		;
;;	Description:	Making a new sequence by giving 3 arguments:		;
;;					Path-Start-Number.									;
;;----------------------------------------------------------------------;
(defun seq-psn (path start num)
  (seq-psen path start nil num)
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-PSEN											;
;;																		;
;;	Description:	Making a new sequence by giving 4 arguments:		;
;;					Path-Start-End-Number.								;
;;----------------------------------------------------------------------;
(defun seq-psen	(path start end num)
  ;; sequence path cannot be PLINE object
  (if (and (path-p path) (path-p start))
	(progn ;; if end-obj is nil, make one
		   (if (not end)
			 (setq end (vla-copy start))
		   )
		   ;; Make sure start-obj and end-obj are identical in
		   ;; object type and number of points.
		   (if (and (object-identical-p start end) (not (sequence-p path)))
			 (add-sequence path start end num)
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	ADD-SEQUENCE										;
;;----------------------------------------------------------------------;
(defun add-sequence	(path start end num-segments / step param child children)
  ;; Move start-obj and end-obj to the start-pt and end-pt of path-obj
  (vla-move start (ais:basepoint start) (ais:startpoint path))
  (vla-move end (ais:basepoint end) (ais:endpoint path))
  ;; make children and attach properties
  (if (< num-segments 1)
	(setq num-segments 1)
  )
  (repeat (1- num-segments)
	(setq child (vla-copy start))
	(setq children (append children (list child)))

	(attach-rog
	  child
	  :rog-interpolate
	  (list start end 'myseqparam)
	)
  )
  ;; ----------------------------------
  ;; Approach #1: Path gets priority
  (attach-rob
	start
	:rob-Basepoint
	(list 'ais:startpoint path)
  )
  (attach-rob end :rob-Basepoint (list 'ais:endpoint path))
  (attach-observer path (list start end))
  ;; ----------------------------------
  ;; Approach #2: StartRef/EndRef get priority
  ;; Problem: Doesn't work for arc object!
  ;;(attach-rob path :rob-StartPoint (list 'ais:basepoint start))
  ;;(attach-rob path :rob-EndPoint (list 'ais:basepoint end))
  (attach-observer start path)
  (attach-observer end path)
  (attach-reactor 'notifier (list path start end))
  ;; Attach properties to the sequence (path object).
  (put-property path :prop-Role :role-Sequence)
  (put-property path :prop-StartRef start)
  (put-property path :prop-EndRef end)
  (put-property path :prop-NumSegments num-segments)
  (vla-put-color path acMagenta)
  (vla-put-color start acCyan)
  (vla-put-color end acCyan)
  ;; Recalculate and record children proportions.
  (seq-add-child path children nil)
  ;;(seq-recalc-children-ppt path)
  ;;(ais-update path)
  ;;(vla-regen (active-document) acActiveViewport)
)


;;----------------------------------------------------------------------;
;;		Function:	SEQ-DISMISS											;
;;----------------------------------------------------------------------;
(defun seq-dismiss (obj)
  (if (sequence-p obj)
	(progn (detach-rob
			 (get-property obj :prop-StartRef)
			 :rob-Basepoint
		   )
		   (detach-rob (get-property obj :prop-EndRef) :rob-Basepoint)
		   (detach-observer obj (get-property obj :prop-StartRef))
		   (detach-observer obj (get-property obj :prop-EndRef))
		   (foreach	c (seq-get-children obj)
			 (detach-observer obj c)
			 (del-property c :prop-MySeqParam)
			 (del-property c :prop-Role)
			 (del-property c :prop-Sequence)
		   )
		   (foreach	p (:prop-StartRef
						:prop-EndRef			  :prop-Children
						:prop-ChildProportion	  :prop-ChildInterpolation
						:prop-Role				  :prop-AisObjectName
					   )
			 (del-property obj p)
		   )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-NORMAL-FOLLOW-PATH								;
;;----------------------------------------------------------------------;
(defun seq-normal-follow-path (obj switch-on)
  (if (sequence-p obj)
	(mapcar	'(lambda (child param)
			   (if (vlax-property-available-p child "Normal")
				 (if switch-on
				   (progn (attach-rob
							child
							:rob-Normal
							(list 'ais:tangent-on-curve-at-ppt obj param)
						  )
						  (vla-put-normal
							child
							(ais:tangent-on-curve-at-ppt obj param)
						  )
						  (vla-update child)
				   )
				   (progn (detach-rob child :rob-Normal)
						  (vla-put-normal child (vlax-3d-point '(0.0 0.0 1.0)))
				   )
				 )
			   )
			 )
			(append	(seq-get-children obj)
					(list (get-property obj :prop-StartRef)
						  (get-property obj :prop-EndRef)
					)
			)
			(append	(get-property obj :prop-ChildProportion)
					(list 0.0 1.0)
			)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-GET-CHILDREN									;
;;																		;
;; Returned value:	Flattened children list.							;
;;					e.g. (B C (D . 0.5) E F) => (B C D E F)				;
;;----------------------------------------------------------------------;
(defun seq-get-children	(seq)
  (if (and (sequence-p seq)
		   (setq children (get-property seq :prop-Children))
	  )
	(mapcar	'(lambda (c)
			   (if (vl-consp c)
				 (car c)
				 c
			   )
			 )
			children
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-GET-FAMILY										;
;;																		;
;; Returned value:	List of parents and children in keyed format.		;
;;					e.g. ((A . 0.0) B C (D . 0.5) E F (G . 1.0))		;
;;----------------------------------------------------------------------;
(defun seq-get-family (seq)
  (if (sequence-p seq)
	(append	(cons (cons (get-property seq :prop-StartRef) 0.0)
				  (get-property seq :prop-Children)
			)
			(list (cons (get-property seq :prop-Endref) 1.0))
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	MAKE-CHILD-IDOL										;
;;																		;
;;	Description:	Make the child the special one.						;
;;----------------------------------------------------------------------;
(defun make-child-idol (seq child / children)
  (if (and (sequence-p seq)
		   (setq children (get-property seq :prop-Children))
		   (member child children)
	  )
	(progn (put-property
			 seq
			 :prop-Children
			 (subst	(cons child
						  (nth (vl-position child children)
							   (get-property seq :prop-ChildProportion)
						  )
					)
					child
					children
			 )
		   )
		   (put-property
			 child
			 :prop-ROG-disabled
			 (get-property child :prop-ROG)
		   )
		   (del-property child :prop-ROG)
		   (vla-put-color child acWhite)
		   (seq-recalc-children-ppt seq)
	  ;; Enable real-time notification
		   (attach-reactor 'notifier child)
		   (attach-observer child seq)
	)
  )
)




;;----------------------------------------------------------------------;
;;		Function:	MAKE-CHILD-MUNDANE									;
;;																		;
;;	Description:	Turn the special child into ordinary.				;
;;----------------------------------------------------------------------;
(defun make-child-mundane (seq child / children m)
  (if (and (sequence-p seq)
		   (setq children (get-property seq :prop-Children))
		   (setq m (vl-member-if
					 '(lambda (x) (and (vl-consp x) (equal (car x) child)))
					 children
				   )
		   )
	  )
	(progn (put-property
			 seq
			 :prop-Children
			 (subst child (car m) children)
		   )
		   (attach-rog
			 child
			 :rog-interpolate
			 (list (get-property seq :prop-StartRef)
				   (get-property seq :prop-EndRef)
				   'MySeqParam
			 )
		   )
		   ;; Remove observer-subject relation
		   (detach-observer child seq)
	  ;; Do not remove child from the reactor since it may still dependable by other's.
		   (vla-put-color child acByLayer)
		   (seq-recalc-children-ppt seq)
	)
  )
)


;;----------------------------------------------------------------------;
;;		Function:	SET-IDOL-CHILD-PARAM								;
;;----------------------------------------------------------------------;
(defun set-idol-child-param	(child param)
  (setq seq (get-property child :prop-Sequence))
  (setq children (get-property seq :prop-Children))
  (put-property
	seq
	:prop-Children
	(subst (cons child param)
		   (cons child (get-property child :prop-MySeqParam))
		   children
	)
  )
  (seq-recalc-children-ppt seq)
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-RECALC-CHILDREN-PPT								;
;;																		;
;;	Description:	Recalculate children's proportion.					;
;;----------------------------------------------------------------------;
(defun seq-recalc-children-ppt (seq / family)
  (setq family (seq-get-family seq))
  ;; Proportion for positions on path object
  (put-property
	seq
	:prop-ChildProportion
	(reverse
	  (cdr (reverse (cdr (seq-propagate-all-ppt family))))
	)
  )
  ;; Proportion for interpolation between key objects.
  (put-property
	seq
	:prop-ChildInterpolation
	(reverse
	  (cdr (reverse (cdr (seq-propagate-all-interp family))))
	)
  )
)

;;	(setq family '((a . 0.0) b c (d . 0.5) e f g h i j (k . 1.0)))
;;	(setq family '((a . 0.0) b c (d . 5.0) e f g h i j (k . 12.0)))

;;----------------------------------------------------------------------;
;;		Function:	SEQ-PROPAGATE-ALL-PPT								;
;;																		;
;;	Description:	Re-propagates parameters (or proportions) based on	;
;;					the family list. This function runs everytime		;
;;					when family list is modified.						;
;;																		;
;;					family = ((a . 0.0) b c d (e . 0.5) f (g . 1.0))	;
;;					=> (0.0 0.125 0.25 0.375 0.5 0.75 1.0)				;
;;----------------------------------------------------------------------;
(defun seq-propagate-all-ppt (family / result step new-param obj-list)
  (setq result nil)
  (foreach item	family
	(if	(vl-consp item)
	  (progn (if (not (null result))
			   (progn (setq	step (/	(float (- (cdr item) (last result)))
									(1+ (length obj-list))
								 )
					  )
					  (foreach obj obj-list
						(setq new-param (+ (last result) step))
						(setq result (append result (list new-param)))
						(if	(vla-object-p obj)
						  (put-property obj :prop-MySeqParam new-param)
						)
					  )
			   )
			 )
			 (setq obj-list nil)
			 (setq result (append result (list (cdr item))))
			 (put-property (car item) :prop-MySeqParam (cdr item))
	  )
	  (setq obj-list (append obj-list (list item)))
	)
  )
  result
)


;;----------------------------------------------------------------------;
;;		Function:	SEQ-PROPAGATE-ALL-INTERP							;
;;----------------------------------------------------------------------;
(defun seq-propagate-all-interp	(family / result step new-param prev-key obj-list)
  (setq result nil)
  (foreach item	family
	(if	(vl-consp item)
	  ;; If item is a key, then...
	  (progn (if (not (null result))
			   (progn (setq step (/ 1.0 (1+ (length obj-list))))
					  (foreach obj obj-list
						(setq new-param (+ (last result) step))
						(setq result (append result (list new-param)))
						(if	(vla-object-p obj)
						  (mod-rog-interpolate obj prev-key (car item) new-param)
						)
					  )
			   )
			 )
			 (setq prev-key (car item))
			 (setq obj-list nil)
			 (setq result (append result (list 0)))
	  )
	  ;; otherwise add item to obj-list for later process.
	  (setq obj-list (append obj-list (list item)))
	)
  )
  result
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-ADD-CHILD										;
;;																		;
;;	Description:	Adding a child or children to a sequence.			;
;;																		;
;;		Arguments:														;
;;			vlaobj	seq		The sequence.								;
;;			mixed	child	The child (VLA object) or children (a list	;
;;							of VLA-object) to be added.					;
;;			float	ppt		Proportion of the insertion point. If nil	;
;;							the child is appened to the end of list.	;
;;----------------------------------------------------------------------;
(defun seq-add-child (seq child ppt / children newchild pos)
  (if (not (vl-consp child))
	(setq child (list child))
  )
  (foreach c child
	(vla-put-layer c :layer-Child)
	(put-property c :prop-Role :role-Child)
	(put-property c :prop-Sequence seq)
	(attach-rob
	  c
	  :rob-Basepoint
	  (list 'ais:point-on-curve-at-ppt seq 'myseqparam)
	)
  )
  ;; Attaching child as observer(s)
  (attach-observer seq child)
  ;; Attaching child reactor
  (attach-reactor 'child child)
  (setq children (seq-get-children seq))
  ;; find position by ppt
  (foreach c child
	(if	(not (member c children))
	  (setq newchild (append newchild (list c)))
	)
  )
  (if (null ppt)
	;; If proportion is not given, find the last suitable position...
	(setq children (append children newchild))
	;;(setq pos (1+
	;;			(vl-position (seq-find-last-ordinary-child seq) children)
	;;		  )
	;;)
	;; If proportion is given, find the position.
	(progn (setq pos (-	(length children)
						(length	(vl-member-if
								  '(lambda (a) (<= ppt a))
								  (get-property seq :prop-ChildProportion)
								)
						)
					 )
		   )
		   (setq children (append (append (left pos children) newchild)
								  (right (- (length children) pos) children)
						  )
		   )
	)
  )
  ;; Insert new child into the children list.
  (put-property seq :prop-Children children)
  (put-property seq :prop-NumSegments (1+ (length children)))
  ;; Recalculate all proportion values.
  (seq-recalc-children-ppt seq)
  (ais-touch seq)
)


;;----------------------------------------------------------------------;
;;		Function:	SEQ-DEL-CHILD										;
;;----------------------------------------------------------------------;
(defun seq-del-child (seq obj / children lst)
  ;; remove from children
  (if (member obj
			  (setq children (get-property seq :prop-Children))
	  )
	;; if ordinary child
	(put-property seq :prop-Children (vl-remove obj children))
	;; if key child
	(if	(setq lst (vl-member-if
					'(lambda (v) (and (vl-consp v) (equal (car v) obj)))
					children
				  )
		)
	  (put-property
		seq
		:prop-Children
		(vl-remove (caar lst) children)
	  )
	)
  )
  ;; remove from observers
  (detach-observer seq obj)
  (put-property
	seq
	:prop-NumSegments
	(length (seq-get-children seq))
  )
  ;; tell the sequence to recalc param
  (seq-recalc-children-ppt seq)
  (ais-touch seq)
)


;;----------------------------------------------------------------------;
;;		Function:	SEQ-DEFAULT-CHILD-PROP								;
;;----------------------------------------------------------------------;
(defun seq-default-child-prop (seq child)
  (put-property child :prop-Sequence seq)
  (bind	child
		:rob-Basepoint
		(list 'ais:point-on-curve-at-ppt seq 'myseqparam)
		t
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-FIND-LAST-ORDINARY-CHILD						;
;;----------------------------------------------------------------------;
(defun seq-find-last-ordinary-child	(seq)
  (car (vl-member-if
		 'vla-object-p
		 (reverse (get-property seq :prop-Children))
	   )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SEQ-ATTACH-TO-CHILDREN								;
;;----------------------------------------------------------------------;


;;----------------------------------------------------------------------;
;;		Function:	SETKEYPARAM											;
;;----------------------------------------------------------------------;
(defun c:setkeyparam (/ obj seq)
  (command-begin)
  (terpri)
  (princ "Key object --> ")
  (setq obj (objsel))
  (terpri)
  (princ "Sequence --> ")
  (setq seq (objsel))
  (terpri)
  (princ "Pick a point on sequence path...")
  (set-idol-child-param
	obj
	(car (pick-n-value-on-curve seq 1 0))
  )
  (ais-update seq)
  (command-end)
)


;; EOF
