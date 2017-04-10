;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-5
;;;;;;;;;;;;;;;;;;;;;;;

;;FACTS
;;; (throttle low) (throttle medium) (throttle high)
;;; (breaks low) (breaks medium) (breaks high)
;;; (car on) (car off) (speed x)
;;; (increment) (decrement)

;;; (control on) (control off)
;;; (fix-speed) (control-speed x)
;;; (pause) (recover)
;;; (increase) (decrease)

;;; (speed-limit x)
;;; (obstacle x)

;(load "C:/Users/deividas/Google Drive/UNIVERSITY/Semester4/KRE/Practical1/level-5.clp")

(defglobal ?*max-speed* = 200)
(defglobal ?*min-obstacle-dist* = 100)


(defrule get-control-speed
?s<-(get-auto-speed-input)
=>
(printout t "Set the desired control speed")
(bind ?input (readline))
(assert (control-speed ?input))

)