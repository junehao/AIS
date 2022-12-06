;; DEMO-BRAMANTE.LSP
(defun c:demo-bramante ()
  (command-begin)
  ;; Global variables
  (setq	center '(0 0 0)
		innerRadius
		 (/ 298.0 2)
		outerRadius
		 (+ innerRadius 294)
		rampCycles 5.5
		floorHeight 380
		columnCycles 4.5
		columnsPerCycle	8
		columnRadius 25
		columnHeight 266
		columnOffset
		 (+ columnRadius 21)
		railHeight 80
  )
  ;; Ramp of the staircase
  (setq	Path (spiral-curve
			   center
			   0
			   innerRadius
			   (* rampCycles 2 pi)
			   (* rampCycles floorHeight)
			   (* 16 totalCycle)
			   1
			 )
  )
  (setq	Path2 (spiral-curve
				center
				0
				outerRadius
				(* rampCycles 2 pi)
				(* rampCycles floorHeight)
				(* 16 totalCycle)
				1
			  )
  )
  (setq	StartRef (vla-addline
				   (model-space)
				   (vlax-3d-point (vlax-curve-getstartpoint Path))
				   (vlax-3d-point (vlax-curve-getstartpoint Path2))
				 )
  )
  (setq	EndRef (vla-addline
				 (model-space)
				 (vlax-3d-point (vlax-curve-getendpoint Path))
				 (vlax-3d-point (vlax-curve-getendpoint Path2))
			   )
  )
  (add-sequence Path StartRef EndRef (* 8 rampCycles))
  (foreach child (seq-get-children Path)
	(bind child
		  "endpoint"
		  (list 'ais:point-on-curve-at-ppt Path2 'myseqparam)
		  nil
	)
  )
  (ais-touch Path)
  ;; Columns
  (setq Path (spiral-curve
			   center
			   0 
			   (+ innerRadius columnOffset)
			   (* columnCycles 2 pi)
			   (* columnCycles floorHeight)
			   (* columnsPerCycle columnCycles)
			   1
			 )
  )
  (setq StartRef (vla-addcircle (model-space) (vlax-3d-point center) columnRadius))
  (setq EndRef (vla-addcircle (model-space) (vlax-3d-point center) columnRadius))
  (add-sequence Path StartRef EndRef (* columnsPerCycle columnCycles))
  (vla-put-thickness StartRef columnHeight)
  (vla-put-thickness StartRef columnHeight)
  (command-end)
)