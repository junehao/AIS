;; DEMO-BRAMANTE.LSP

;; Global variables
(setq innerRadius (/ 298.0 2))
(setq outerRadius (+ innerRadius 294))
(setq totalCycle 5.5)
(setq totalHeight (* totalCycle 310))
(setq totalAngle (* totalCycle 2 pi))
(setq columnsPerCycle 8)
(setq columnRadius 25)
(setq columnHeight 266)
(setq columnOffset (+ columnRadius 21))
(setq railHeight 80)

;; Create spiral lines
(setq innerSpiral
	   (spiral-curve
		 '(0 0 0)
		 0
		 innerRadius
		 totalAngle
		 totalHeight
		 (* 6 totalCycle)
		 2
	   )
)

(setq outerSpiral
	   (spiral-curve
		 '(0 0 0)
		 0
		 outerRadius
		 totalAngle
		 totalHeight
		 (* 6 totalCycle)
		 2
	   )
)

;; Create sequence
(sequence-2path
  innerSpiral
  outerSpiral
  (* columnsPerCycle totalCycle)
)

