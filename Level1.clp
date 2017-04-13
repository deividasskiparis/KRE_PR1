;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-1
;;;;;;;;;;;;;;;;;;;;;;;

;;FACTS
;;; (throttle low) (trhottle medium) (throttle high)
;;; (breaks low) (breaks medium) (breaks high)
;;; (car on) (car off) (speed x)
;;; (increment) (decrement)

;;; (control on) (control off)
;;; (cruise on) (cruise paused)
;;; (fix-speed) (cruise-speed x)
;;; (pause) (recover)
;;; (increase) (decrease)


(defglobal ?*max-speed* = 200)
(defglobal ?*min-cruise-speed* = 50)

; The driver can activate/deactivate a cruise control system with (control on)/(control off)
; Control cannot get activated if the care speed is below 50 Km/h.

(defrule control-on
  ?on <- (control on)
  (speed ?s&:(> ?s ?*min-cruise-speed*))
  =>
  (assert (cruise on))
  (retract ?on)
  (printout t "Cruise control on." crlf)
)

(defrule control-on-low-speed
  (declare (salience -1))
  ?on <- (control on)
  =>
  (retract ?on)
)

(defrule control-off
  ?off <- (control off)
  ?cruise <- (cruise ?)
  =>
  (retract ?off ?cruise)
  (printout t "Cruise control off." crlf)
)

; Cruise control deactivates automatically if these low speeds [50 Km/h] are reached.

(defrule control-off-automatic
  (declare (salience 2))
  ?cruise <- (cruise ?)
  (speed ?s&:(< ?s ?*min-cruise-speed*))
  =>
  (retract ?cruise)
  (printout t "Cruise control off (automatic)." crlf)
)

; With the cruise contol activated, the driver can press (fix-speed) button, which means that
; the current speed is fixed to be the cruise speed (cruise-speed X).

(defrule fix-speed
  (declare (salience 2))
  (cruise on)
  ?fspeed <- (fix-speed)
  (speed ?s&:(> ?s ?*min-cruise-speed*)) ;senseless to have cruise control below minimal cruise speed.
  =>
  (assert (cruise-speed ?s))
  (retract ?fspeed)
  (printout t "Cruise speed fixed to " ?s " Km/h." crlf)
)

; when (fix-speed) pressed and rule fix-speed does not activate, retract (fix-speed)
(defrule fix-speed-too-low
  (declare (salience 0))
  ;(cruise on) ; get rid of (fix-speed) even if cruise is not activated
  ?fspeed <- (fix-speed)
  (speed ?)
  =>
  (retract ?fspeed)
)

; when the cruise control is not activated, (fix-speed) is not considered
(defrule fix-speed-cruise-not-activated
   (not (cruise on))
   ?fspeed <- (fix-speed)
   =>
   (retract ?fspeed)
)

; If the driver pushes throttle/breaks or the (pause) button, the cruise control is paused
; an the system starts behaving at level-0 control.

(defrule throttle-or-breaks-with-cruise
  (declare (salience 2))
  (cruise ?)
  (or (throttle ?) (breaks ?))
  =>
  (assert (cruise paused))
  ;neither throttle nor breaks are removed to get speed reduction with level-0 rules throttle/breaks.
)

(defrule pause-cruise
  (declare (salience 2))
  ?cruise <- (cruise ?)
  ?pause <- (pause)
  =>
  (retract ?pause ?cruise)
  (assert (cruise paused))
  (printout t "The cruise control is paused." crlf)
)

; From a paused situation, the driver can recover cruise control by pushing the button (recover), ; then the cruise control will start working to increase/decrease the current speed to reach the
; cruise speed by means of instructions (increment)/(decrement).

(defrule recover-cruise
  (declare (salience 2))
  ?pcruise <-(cruise paused)
  ?recover <- (recover)
  =>
  (retract ?pcruise ?recover)
  (assert (cruise on))
  (printout t "Cruise speed is being recovered." crlf)
)

(defrule automatic-increase-speed
  (cruise on)
  (speed ?speed)
  (cruise-speed ?cruise-speed&:(> ?cruise-speed ?speed))
  (not (increment))
  =>
  (assert (increment))
  (printout t "(increment) sent." crlf)
)

(defrule automatic-decrease-speed
  (cruise on)
  (speed ?speed)
  (cruise-speed ?cruise-speed&:(< ?cruise-speed ?speed))
  (not (decrement))
  =>
  (assert (decrement))
  (printout t "(decrement) sent." crlf)
)

; If the speed goes below 50 Km/h in pause mode, the cruise control should be automatically deactivated

(defrule automatic-low-speed-paused-deactivation
  ?cruise <- (cruise paused)
  (speed ?s&:(< ?s ?*min-cruise-speed*))
  =>
  (retract ?cruise)
)

; While cruise control is activated, the user can increase/decrease speed by 5 Km/h after
; pushing (increase)/(decrease) buttons.

(defrule increase
  (cruise on)
  ?increase <- (increase)
  ?speed <- (speed ?s&:(< ?s (- ?*max-speed* 5)))
  =>
  (retract ?increase ?speed)
  (assert (speed (+ ?s 5)))
)

(defrule decrease
  (cruise on)
  ?decrease <- (decrease)
  ?speed <- (speed ?s)
  =>
  (retract ?decrease ?speed)
  (assert (speed (- ?s 5)))
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-0
;;;;;;;;;;;;;;;;;;;;;;;

;;FACTS
;;; (throttle low) (trhottle medium) (throttle high)
;;; (breaks low) (breaks medium) (breaks high)
;;; (car on) (car off) (speed x)
;;; (increment) (decrement)

(defrule car-on
  ?on <- (car on)
  =>
  (retract ?on)
  (assert (speed 0))
  (printout t "Car is started." crlf)
)

(defrule car-off
  ?speed <- (speed 0)
  ?off <- (car off)
  =>
  (retract ?speed ?off)
  (printout t "Car is stopped." crlf)
)

(defrule car-off-not-stopped
  (speed ?s&:(> ?s 0))
  ?off <- (car off)
  =>
  (retract ?off)
)


(deffunction times (?i)
  (switch ?i
    (case low then (bind ?result 5))
    (case medium then (bind ?result 10))
    (case high then (bind ?result 15))
    (default (bind ?result 0)))
  (return ?result)
)

(defrule breaks
  ?breaks <- (breaks ?intensity)
  (speed ?s&:(> ?s 0))
  =>
  (assert (decrement (times ?intensity)))
  (retract ?breaks)
)

(defrule breaks-stopped
  ?breaks <- (breaks ?)
  (speed 0)
  =>
  (retract ?breaks)
)

(defrule throttle
  ?throttle <- (throttle ?intensity)
  (speed ?s&:(< ?s 200))
  =>
  (assert (increment (times ?intensity)))
  (retract ?throttle)
)

(defrule throttle-stopped
  ?throttle <- (throttle ?)
  (speed 200)
  =>
  (retract ?throttle)
)

(defrule increments
(declare (salience 1))
  ?i <- (increment ?num&:(>= ?num 5))
  =>
  (assert (increment) (increment (- ?num 5)))
  (printout t "(increment) sent." crlf)
  (retract ?i)
)

(defrule stop-increments
  (declare (salience 1))
  ?i <- (increment 0)
  =>
  (retract ?i)
)

(defrule decrements
  (declare (salience 1))
  ?i <- (decrement ?num&:(>= ?num 5))
  =>
  (assert (decrement) (decrement (- ?num 5)))
  (printout t "(decrement) sent." crlf)
  (retract ?i)
)

(defrule stop-decrements
  (declare (salience 1))
  ?i <- (decrement 0)
  =>
  (retract ?i)
)

;; EXTERNAL

(defrule speed-update-1
  (declare (salience 2))
  ?incr  <- (increment)
  ?speed <- (speed ?s)
  =>
  (retract ?incr ?speed)
  (assert (speed (min (+ ?s 5) ?*max-speed*)))
)

(defrule speed-update-2
  (declare (salience 2))
  ?decr  <- (decrement)
  ?speed <- (speed ?s)
  =>
  (retract ?decr ?speed)
  (assert (speed (max (- ?s 5) 0)))
)

