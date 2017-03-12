(deftemplate car
	(slot state
		(type SYMBOL)
		(allowed-symbols on off)
    		(default off))
	(slot speed
		(type FLOAT)
    		(default 0.0))	
	(slot action
		(type SYMBOL)
		(allowed-symbols None on off acc brk)
    		(default None))	
	(slot intensity
		(type SYMBOL)
		(allowed-symbols None low med hi)
    		(default None))
)


(deffacts cars "Initialize a car"
	(car (state off))
)

(defrule car_on
	?f<-(driver_input on)
	?c<-(car (state off))
	=> 
	(printout t "Car on" crlf)
	(retract ?f); retract the input signal
	(modify ?c (state on))
)
(defrule car_on_false
	?f<-(driver_input on)
	?c<-(car (state on))
	=> 
	(retract ?f); retract the input signal
)

(defrule car_off_false
	?f<-(driver_input off)
	?c<-(car (state off))
	=> 
	(retract ?f); retract the input signal
)

(defrule car_off
	?f<-(driver_input off)
	?c<-(car (state on) (speed 0.0))
	=> 
	(printout t "Car off" crlf)
	(retract ?f); retract the input signal
	(modify ?c (state off))
)
(defrule speed_up
	?f<-(driver_input acc ?intensity)
	?c<-(car (state on) (speed ?sp))
	=> 
	(switch ?intensity
	  (case low then (modify ?c (speed (min (max (* ?sp 1.05) 5) 200.0))))
	  (case med then (modify ?c (speed (min (max (* ?sp 1.1) 5) 200.0))))
	  (case hi then (modify ?c (speed (min (max (* ?sp 1.15) 5) 200.0))))
	  (default (printout t "No such intensity: " ?intensity crlf))
	)
	(printout t "New speed: " ?sp crlf)
)

(defrule stop_speed_up
	?f<-(driver_input acc ?intensity)
	(car (state on) (speed 200.0))
	=>
	(printout t "New speed: 200.0" crlf)
	(retract ?f); retract the input signal

)

(defrule speed_down
	?f<-(driver_input brk ?intensity)
	?c<-(car (state on) (speed ?sp))
	=> 
	(switch ?intensity
	  (case low then (modify ?c (speed (max (max (* ?sp 0.95) 1.0) 0.0))))
	  (case med then (modify ?c (speed (max (max (* ?sp 0.9) 1.0) 0.0))))
	  (case hi then (modify ?c (speed (max (max (* ?sp 0.85) 1.0) 0.0))))
	  (default (printout t "No such intensity: " ?intensity crlf)
			(retract ?f); retract the input signal
			)
	)
	(printout t "New speed: " ?sp crlf)
)

(defrule stop_speed_down
	?f<-(driver_input brk ?intensity)
	?c<-(car (state on) (speed 1.0))
	=>
	(retract ?f); retract the input signal
	(modify ?c (speed 0.0))
	(printout t "New speed: 0" crlf)

)


(defrule delete_old_inputs
(declare (salience 99))
?new <- (driver_input ?type $?)
?olds <- (driver_input ~?type $?)
;?old-list <- (list $?front ?who $?rear)
=>
(retract ?olds)
)

(defrule delete_old_inputs2
(declare (salience 99))
?new <- (driver_input ?type ?intensity)
?olds <- (driver_input ?type ~?intensity)
;?old-list <- (list $?front ?who $?rear)
=>
(retract ?olds)
)