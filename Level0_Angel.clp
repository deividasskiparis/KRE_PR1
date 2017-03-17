; The driver can press (throttle X) and (brakes X) being X intensity low, medium, or high. DONE
; The car can be on/off (car on)/(car off) and its speed (speed X) known in Km/h. DONE
; As the system receives throttle/brakes/on/off signals, the electronic control system controls the speed by means of (increment) and (decrement) signals, each one producing a 5 Km/h increment/decrement of the current speed. DONE
; Signals on/off are only attended if speed is 0 km/h. DONE
; For low/medium/high throttle/brakes intensities, speed is expected to increase/decrease 5/10/15 Km/h (approximately) the current speed by means of (increment) or (decrement) signals. ALMOST DONE
; Speed must always be between 0 and 200 Km/h. DONE

; SUPPOSITIONS
; The car can be either on or off at the same time
; The driver can press either throttle or brakes at the same time

;IDEA: (car <throttle> <brakes> <state> <speed>)

; (throttle X)
; (allowed-strings "remain" "low" "medium" "high") ;improve
; (brakes X)
; (allowed-strings "remain" "low" "medium" "high") ;improve
; (car state)
; (allowed-strings "on" "off") ;improve
; (speed X)
; (range 0 200) ;improve

; Initial set of facts:
(assert 
        (car "off")
        (throttle "remain")
        (brakes "remain")
        (speed 0))

; Rules for turn on/off the car:
(defrule turn-on-car
        "The driver turns on the car"
        ;(declare (salience 0)) ; medium priority –default 0
        ?car <- (car "off")
        (speed 0)
        =>
        (retract ?car)
        (assert (car "on"))
        (printout t "The car is turned on.” crlf))

(defrule turn-on-car
        "The driver turns off the car"
        ;(declare (salience 0)) ; medium priority –default 0
        ?car <- (car "on")
        (speed 0)
        =>
        (retract ?car)
        (assert (car "off"))
        (printout t "The car is turned off.” crlf))

; Rules for increase/decrease the speed:
(defrule increase
        "The car needs to increase the speed by 5 km/h."
        ;(declare (salience 0)) ; medium priority –default 0
        (car "on")
        ?speed <- (speed ?current-speed)
        (< ?current-speed 200)
        =>
        (retract ?speed)
        (assert (speed (+ ?current-speed 5)))
        (printout t "The car increase speed by 5 km/h.” crlf))

(defrule decrease
        "The car needs to decrease the speed by 5 km/h."
        ;(declare (salience 0)) ; medium priority –default 0
        (car "on")
        ?speed <- (speed ?current-speed)
        (> ?current-speed 0)
        =>
        (retract ?speed)
        (assert (speed (- ?current-speed 5)))
        (printout t "The car decrease speed by 5 km/h.” crlf))

; (throttle "low")
(increase)

; (throttle "medium")
(increase)
(increase)

; (throttle "high")
(increase)
(increase)
(increase)

; (brakes "low")
(decrease)

; (brakes "medium")
(decrease)
(decrease)

; (brakes "high")
(decrease)
(decrease)
(decrease)