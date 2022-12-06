;;		CORE.LSP														;
;;		By June-Hao Hou, updated Feb. 27, 2006.							;
(vl-load-com)

;;----------------------------------------------------------------------;
;; Global flags															;
;;----------------------------------------------------------------------;
(setq *ais-reset* nil		  ; Entering RESET mode?
	  *ais-debug* nil		  ; Entering DEBUG mode?
)

;;----------------------------------------------------------------------;
;; Global variables and constants (core)								;
;;----------------------------------------------------------------------;
(setq *acad-object*	nil
	  *active-document*	nil
	  *model-space*	nil
	  save-cmdecho nil
	  save-snapmode	nil
	  util nil
)

(setvar "ZOOMFACTOR" 20)	  ; (RW) default 60
(setvar "TRACKPATH" 0)		  ; (RW) default 0

;;----------------------------------------------------------------------;
;;		Function:	AIS-DEBUG											;
;;----------------------------------------------------------------------;
(defun ais-debug (on)
  (if on
	(progn (setq *ais-debug* t)
		   (setq *ais-modified-calls* 0)
		   (setq *ais-updates* 0)
	)
	(setq *ais-debug* nil)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	C:AIS-INIT											;
;;																		;
;;	 Description:	AIS system initialization							;
;;----------------------------------------------------------------------;
(defun c:ais-init ()
  (if (not *ais-initialized*)
	(progn ;; Linking model space
		   (model-space)
		   ;; Linking VLA Utility
		   (setq util (vla-get-utility (active-document)))
		   ;; Adding required layers
		   (ais-init-layers)
		   ;; adding required symbols
		   (crosshair)
		   (ais-load-menugroup)
		   (ais-reactor-setup)
		   (setq *ais-initialized* t)
		   (setq *ais-reaction-on* t)
		   (terpri)
		   (princ "AIS3 is initialized.")
		   (princ)
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-RESET											;
;;																		;
;;	 Description:	Reset all AIS system settings and global variables	;
;;----------------------------------------------------------------------;
(defun c:ais-reset ()
  (setq *ais-reset* t)		  ; begin init process
  (c:ais-init)
  (cleanup-ldata)
  (ais-init-globals)
  (vlr-remove-all)
  (setq *ais-reset* nil)	  ; end init process
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-INIT-LAYERS										;
;;----------------------------------------------------------------------;
(defun ais-init-layers ()
  (add-layer "AIS-Label")
  (layer-setColor "AIS-Label" 3) ; green
  (add-layer "AIS-IB")
  (layer-setColor "AIS-IB" 252) ; dark grey
  (add-layer "AIS-Sequence")
  (layer-setColor "AIS-Sequence" 6) ; magenta
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-INIT-GLOBALS									;
;;----------------------------------------------------------------------;
(defun ais-init-globals	()
  ;; Cleanup global variables
  (setq *aisr-notifier* nil)
  (setq *ais-objectsToModify* nil)
  (setq *ais-objectsModifying* nil)
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-LOAD-TOOLBAR									;
;;----------------------------------------------------------------------;
(defun ais-load-menugroup (/ mgroup)
  ;; If AIS Toolset menugroup is not loaded, then...
  (if (not (menugroup "AIS TOOLSET"))
	;; Load in the menugroup...
	(if	(setq mgroup (vla-load (vla-get-menugroups (acad-object))
							   "AIS_TOOLSET.mnu"
					 )
		)
	  ;; and show the toolbar.
	  (vla-put-visible
		(vla-item (vla-get-toolbars mgroup) 0)
		:vlax-true
	  )
	)
  )
)

;;//////////////////////////////////////////////////////////////////////;
;;		List Operations													;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	LEFT												;
;;																		;
;;	 Description:	Returns the left N items of a list.					;
;;----------------------------------------------------------------------;
(defun left	(n lst)
  (if (<= n (length lst))
	(cond ((<= n 0) nil)
		  ((= n 1) (list (car lst)))
		  (T (append (left (1- n) lst) (list (nth (1- n) lst))))
	)
	lst
  )
)

;;----------------------------------------------------------------------;
;;		Function:	RIGHT												;
;;																		;
;;	 Description:	Returns the right N items of a list.				;
;;----------------------------------------------------------------------;
(defun right (n lst) (reverse (left n (reverse lst))))

;;----------------------------------------------------------------------;
;;		Function:	SUBLIST												;
;;																		;
;;	 Description:	Returns a sub-list by index range.					;
;;----------------------------------------------------------------------;
(defun sublist (lst from to)
  (if (and (<= from to) (>= from 0))
	(left (1+ (- to from)) (right (- (length lst) from) lst))
	nil
  )
)

;;----------------------------------------------------------------------;
;;		Function:	SETQ-MAP											;
;;																		;
;;	 Description:	Variable assignment by mapping values to symbols.	;
;;																		;
;;		Usage:		(setq-map '(var1 var2...) '(value1 value2...))		;
;;					var1 = value1										;
;;					var2 = value2...									;
;;----------------------------------------------------------------------;
(defun setq-map (A B) (mapcar 'set A B))

;;----------------------------------------------------------------------;
;;		Function:	CHUNK												;
;;																		;
;;		Usage:		(chunk '(a b c d e f) 2)							;
;;					=> ((a b) (c d) (d f))								;
;;----------------------------------------------------------------------;
(defun chunk (lst num)
  (if (null lst)
	nil
	(cond ((= (length lst) num) (list lst))
		  (T
		   (cons (left num lst)
				 (chunk (right (- (length lst) num) lst) num)
		   )
		  )
	)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	EVERY												;
;;																		;
;;		Usage:		(every 2 '(a b c d e))	=> (a c e)					;
;;----------------------------------------------------------------------;
(defun every (num lst)
  (mapcar '(lambda (n) (nth n lst))
		  (series 0 (1- (length lst)) num)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	LIST-MAKE-UNIQUE									;
;;																		;
;;	Description:	This function remove duplicated atoms and retain	;
;;					the first one, i.e. make each atom unique. This		;
;;					is the wrapper function of list-make-unique*.		;
;;																		;
;;	Requirements:	list-make-unique*									;
;;																		;
;;		Usage:		(list-make-unique '(a b a c d e b d))				;
;;					=> (a b c d e)										;
;;----------------------------------------------------------------------;
(defun list-make-unique	(lst)
  (list-make-unique* (car lst) (cdr lst))
)

;;----------------------------------------------------------------------;
;;		Function:	LIST-MAKE-UNIQUE*									;
;;																		;
;;	 Description:	This is the actual recursive function for			;
;;					list-make-unique.									;
;;----------------------------------------------------------------------;
(defun list-make-unique* (head tail)
  (cond	((null tail) (list head))
		(t
		 (if (member head tail)
		   (list-make-unique* head (vl-remove head tail))
		   (cons head (list-make-unique* (car tail) (cdr tail)))
		 )
		)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	INSERT												;
;;----------------------------------------------------------------------;
(defun insert (new idx lst)
  (append (append (left idx lst) (list new)) (right (- (length lst) idx) lst)))


;;----------------------------------------------------------------------;
;;		Function:	STR-IMPLODE											;
;;																		;
;;	 Description:	Join a list of strings and seperate by the given	;
;;					delimiter.											;
;;																		;
;;		Usage:		(str-implode " " '("A" "BC" "D"))					;
;;					=> "A BC D"											;
;;----------------------------------------------------------------------;
(defun str-implode (delim lst)
  (if (null (cdr lst))
	(car lst)
	(strcat (car lst) delim (str-implode delim (cdr lst)))
  )
)

;;//////////////////////////////////////////////////////////////////////;
;;		Error Handlers													;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	*ERROR*												;
;;																		;
;;	 Description:	A generic error handler.							;
;;----------------------------------------------------------------------;
(defun *error* (msg)
  (princ (strcat "Error: " msg))
  (terpri)
)

;;----------------------------------------------------------------------;
;;		Function:	ERROR-AIS-NOT-INITIALIZED							;
;;----------------------------------------------------------------------;
(defun error-ais-not-initialized	()
  (alert "Type AIS-INIT to initialize AIS system.")
)


;;//////////////////////////////////////////////////////////////////////;
;;		Data conversion													;
;;//////////////////////////////////////////////////////////////////////;

;;----------------------------------------------------------------------;
;;		Function:	LSP-BOOL											;
;;																		;
;; 	 Description:	Map to LISP T/nil.									;
;;----------------------------------------------------------------------;
(defun lsp-bool	(bool)
  (or (= bool acTrue) (= bool :vlax-true))
)

;;----------------------------------------------------------------------;
;;		Function:	AC-BOOL												;
;;																		;
;;	 description:	Map to ACAD Boolean.								;
;;----------------------------------------------------------------------;
(defun ac-bool (bool)
  (if (or (= bool t) (= bool :vlax-true))
	acTrue
	acFalse
  )
)

;;----------------------------------------------------------------------;
;;		Function:	VLAX-BOOL											;
;;																		;
;;	 Dexcription:	Map to VLAX Boolean.								;
;;----------------------------------------------------------------------;
(defun vlax-bool (bool)
  (if (or (= bool t) (= bool acTrue))
	:vlax-true
	:vlax-false
  )
)

;;----------------------------------------------------------------------;
;;		Function:	STRING-P											;
;;----------------------------------------------------------------------;
(defun string-p (var) (eq 'STR (type var)))

;; EOF