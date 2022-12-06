;;		AIS.LSP															;
;;		AIS Dispatcher Functions										;
;;		By June-Hao Hou, Dec 2022										;
(vl-load-com)

;;----------------------------------------------------------------------;
;; Global Variables													                            ;
;;----------------------------------------------------------------------;
(setq *ais-property-type* 
       '( ("ArcLength" . "float")
          ("Basepoint" . "variant")
          ("Bulge" . "float")
          ("Center" . "variant")
          ("ControlPoints" . "variant")
          ("Coordinate" . "variant")
          ("Coordinates" . "variant")
          ("Diameter" . "float")
          ("Elevation" . "float")
          ("EndAngle" . "float")
          ("EndPoint" . "variant")
          ("EndTangent" . "variant")
          ("FitPoints" . "variant")
          ("Length" . "float")
          ("Normal" . "variant")
          ("Radius" . "float")
          ("StartAngle" . "float")
          ("StartPoint" . "variant")
          ("StartTangent" . "variant")
          ("Thickness" . "float")
          ("TotalAngle" . "float")
        )
)

;;----------------------------------------------------------------------;
;;  Function: AIS-TYPE											                          ;
;;  Return AIS object's geometry type.
;;----------------------------------------------------------------------;
(defun ais-type (obj) 
  (cond 
    ((arc-p obj) "arc")
    ((circle-p obj) "circle")
    ((line-p obj) "line")
    ((pline-p obj) "pline")
    ((pline3d-p obj) "pline3d")
    ((point-p obj) "point")
    ((spline-p obj) "spline")
  )
)

;;----------------------------------------------------------------------;
;;  Function:	AVAILABLE-INLETS									                        ;
;;----------------------------------------------------------------------;
(defun available-inlets (obj) 
  (eval (read (strcat "*" (ais-type obj) "-inlets*")))
)

;;----------------------------------------------------------------------;
;;  Function:	AVAILABLE-OUTLETS									                        ;
;;----------------------------------------------------------------------;
(defun available-outlets (obj datatype / result) 
  (foreach o (eval (read (strcat "*" (ais-type obj) "-outlets*"))) 
    (if 
      (equal (strcase (cdr (assoc o *ais-property-type*))) 
             (strcase datatype)
      )
      (setq result (append result (list o)))
    )
  )
  result
)

;;----------------------------------------------------------------------;
;;  Function:	AIS-TOUCH											                            ;
;;  Set object visibility to True.
;;----------------------------------------------------------------------;
(defun ais-touch (obj) 
  (if (vla-object-p obj) 
    (vla-put-visible obj acTrue)
  )
)

;;----------------------------------------------------------------------;
;;  Function:	AIS-UPDATE											                          ;
;;																		                                  ;
;;	Requirements:	ais-update-rog, ais-update-rob										    ;
;;  @returns none
;;----------------------------------------------------------------------;
(defun ais-update (obj / rogs robs) 
  (if *ais-debug* 
    (setq *ais-updates* (1+ *ais-updates*))
  )
  ;|(if (null *ais-update-initiater*)
	(setq *ais-update-initiater* obj)
  )|;
  (if (vla-object-p obj) 
    (progn  ;; DEBUG: Highlight and wait
           (if *ais-debug* 
             (progn (crosshair-show) 
                    (crosshair-move (VtoL (ais:basepoint obj)))
                    (getstring)
             )
           )
           ;; Update object
           (if (vlax-write-enabled-p obj) 
             (progn (ais-update-rog obj) (ais-update-rob obj))
           )
    )
    (if (vl-consp obj) 
      (progn (vlr-add (car obj)) 
             (if *ais-debug* 
               (princ "*reactor ON*")
             )
      )
    )
  )
  ;; Be cautious here!!!!! May become deep iteration!

  ;|
  (if (sequence-p obj)
	  (foreach o (get-property obj :prop-Observer)
	    (if (not (equal *ais-update-initiater* o))
		    (ais-update o)
      )
    )
  )
  (if (equal *ais-update-initiater* obj)
	  (setq *ais-update-initiater* nil)
  )
  |;
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROG										                      ;
;;																		                                  ;
;;	Requirements:	<Type>-interpolate-update							                ;
;;					<Type>-derive-update								                        ;
;;----------------------------------------------------------------------;
(defun ais-update-rog (obj / rog func symtype) 
  (setq myseqparam (get-property obj :prop-MySeqParam))
  (if 
    (and (setq rog (get-property obj :prop-ROG)) 
         (setq args (cons obj (cdr rog)))
    )
    (cond 
      ((eq :rog-interpolate (car rog))
       (setq func (read (strcat (ais-type obj) "-interpolate-update")))
       (setq symtype (type (vl-symbol-value func)))
       (if (or (eq symtype 'USUBR) (eq symtype 'SUBR)) 
         (apply func args)
       )
      )
      ((eq :rog-derive (car rog))
       (setq func (read (strcat (ais-type obj) "-derive-update")))
       (setq symtype (type (vl-symbol-value func)))
       (if (or (eq symtype 'USUBR) (eq symtype 'SUBR)) 
         (apply func args)
       )
      )
    )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB										                      ;
;;																		                                  ;
;;	Requirements:	purge-orphen-rob								  	                  ;
;;					ais:basepoint										                            ;
;;					ais:midpoint										                            ;
;;					ais-update-rob-*									                          ;
;;----------------------------------------------------------------------;
(defun ais-update-rob (obj / robs prop expr eval-expr) 
  ;; Clean up orphen ROBs
  (purge-orphen-rob obj)
  ;; Load my sequence param into a global variable to be used by expr.
  (setq myseqparam (get-property obj :prop-MySeqParam))
  (if (setq robs (get-property obj :prop-ROB)) 
    ;; For each ROB in the ROB list...
    (foreach rob robs 
      ;; If the property is available...
      (if 
        (and (setq prop (car rob)) 
             (setq expr (cdr rob))
             (vlax-property-available-p obj prop)
        )
        ;; Then if ROB expression is a VLA object...
        (if 
          (and (not (vl-consp expr)) 
               (vla-object-p expr)
               (vlax-read-enabled-p expr)
               (vlax-property-available-p expr prop)
          )
          ;; Then do a direct copy of the property value.
          (vlax-put-property obj prop (vlax-get-property expr prop))
          ;; Otherwise, evaluate the ROB Data to get property value.
          (vlax-put-property obj prop (eval expr))
        )
        ;; Otherwise (when ROB expression is just an expression)
        ;; If expr can be directly evaluated...
        (if 
          (and (vl-symbolp (car expr)) 
               (or (eq (type (vl-symbol-value (car expr))) 'SUBR) 
                   (eq (type (vl-symbol-value (car expr))) 'USUBR)
               )
               (setq eval-expr (eval expr))
          )
          ;; Then perform actions according to the property.
          (apply (read (strcat "ais-update-rob-" prop)) (list obj eval-expr))
          ;; Otherwise (when expr contains various form of arguments)...
          (cond  ;; Set bulge to a specific vertex at given index.
            ;; !!! Usefulness?
            ((eq "BULGE" (strcase prop))
             (ais-update-rob-bulge obj (car expr) (eval (cdr expr)))
            )
          )
        )
      )
    )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-BASEPOINT							;
;;----------------------------------------------------------------------;
(defun ais-update-rob-basepoint (obj point) 
  (vla-move obj (ais:basepoint obj) point)
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-MIDPOINT								;
;;----------------------------------------------------------------------;
(defun ais-update-rob-midpoint (obj point) 
  (vla-move obj (ais:midpoint obj) point)
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-CENTER								;
;;----------------------------------------------------------------------;
(defun ais-update-rob-center (obj center) 
  (vla-move obj (ais:center obj) center)
  (if (spiral-p obj) 
    (put-property obj :prop-Center (VtoL center))
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-STARTPOINT							;
;;----------------------------------------------------------------------;
(defun ais-update-rob-startpoint (obj point) 
  (cond 
    ((pline-p obj)
     (vla-put-coordinate obj 0 (ais:2d-point point))
     ;;(vla-put-elevation obj (ais:get-z point))
    )
    ((pline3d-p obj) (vla-put-coordinate obj 0 point))
    ;; Fits points in Spline are problematic!
    ((spline-p obj)
     (if (> (vla-get-numberoffitpoints obj) 0) 
       (vla-setfitpoint obj 0 point)
       (vla-setcontrolpoint obj 0 point)
     )
    )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-ENDPOINT								;
;;----------------------------------------------------------------------;
(defun ais-update-rob-endpoint (obj point) 
  (cond 
    ((pline-p obj)
     (vla-put-coordinate 
       obj
       (1- (/ (length (VtoL (vla-get-coordinates obj))) 2))
       (ais:2d-point point)
     )
    )
    ((pline3d-p obj)
     (vla-put-coordinate 
       obj
       (1- (/ (length (VtoL (vla-get-coordinates obj))) 3))
       point
     )
    )
    ;; Fits points in Spline are problematic!
    ((spline-p obj)
     (if (> (vla-get-numberoffitpoints obj) 0) 
       (vla-setfitpoint 
         obj
         (1- (vla-get-numberoffitpoints obj))
         point
       )
       (vla-setcontrolpoint 
         obj
         (1- (vla-get-numberofcontrolpoints obj))
         point
       )
     )
    )
  )
)


;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-ELEVATION							;
;;----------------------------------------------------------------------;
(defun ais-update-rob-elevation (obj value) 
  (cond 
    ((pline-p obj) (vla-put-elevation obj value))
    (t
     (vla-move obj 
               (basepoint obj)
               (LtoV 
                 (append (left 2 (VtoL (basepoint obj))) 
                         (list value)
                 )
               )
     )
    )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-BULGE								;
;;																		;
;;	Description:	This function works for PLINE object only.			;
;;----------------------------------------------------------------------;
(defun ais-update-rob-bulge (obj index bulge) 
  (if (pline-p obj) 
    (vla-setbulge obj index bulge)
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-ROB-RADIUS								;
;;																		;
;;	Description:	This function works for SPIRAL curve only.			;
;;----------------------------------------------------------------------;
(defun ais-update-rob-radius (obj radius) 
  (if (spiral-p obj) 
    (progn (put-property obj :prop-Radius radius) 
           (spiral-update obj)
    )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-DERIVE											                        ;
;;----------------------------------------------------------------------;
(defun ais-derive (obj from to / func symtype) 
  (if (and (vla-object-p obj) (vlax-read-enabled-p obj)) 
    (progn (setq func (read (strcat (ais-type obj) "-derive"))) 
           (setq symtype (type (eval func)))
           (if (or (eq symtype 'USUBR) (eq symtype 'SUBR)) 
             (apply func (list obj from to))
           )
    )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-INTERPOLATE										                      ;
;;----------------------------------------------------------------------;
(defun ais-interpolate (obj1 obj2 param / func) 
  (if 
    (and (vla-object-p obj1) 
         (vla-object-p obj2)
         (vlax-read-enabled-p obj1)
         (vlax-read-enabled-p obj2)
         (object-identical-p obj1 obj2)
    )
    (progn (setq func (read (strcat (ais-type obj1) "-interpolate"))) 
           (setq symtype (type (eval func)))
           (if (or (eq symtype 'USUBR) (eq symtype 'SUBR)) 
             (apply func (list obj1 obj2 param))
           )
    )
  )
)

;;----------------------------------------------------------------------;
;;		Function:	AIS-UPDATE-TRACE									                      ;
;;																		                                  ;
;;	Description:	Trace the order of change of last upadte.			        ;
;;					For debugging purpose.								                      ;
;;----------------------------------------------------------------------;
(defun ais-update-trace () 
  (crosshair-show)
  (if *ais-objectsModifying* 
    (foreach obj *ais-objectsModifying* 
      (if (vla-object-p obj) 
        (progn (crosshair-move (VtoL (ais:basepoint obj))) 
               (vla-put-color (crosshair) (vla-get-color obj))
               (getstring)
        )
      )
    )
  )
  (crosshair-hide)
)

;; EOF