;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-5
;;;;;;;;;;;;;;;;;;;;;;;

;;FACTS
;;; (throttle low) (throttle medium) (throttle high)
;;; (breaks low) (breaks medium) (breaks high)
;;; (car on) (car off) (speed x)
;;; (increment) (decrement)

;;; (control on) (control off)
;;; (control-speed x)
;;; (set-control-speed x)

;;; (speed-limit x)
;;; (obstacle x)

(defglobal ?*max-speed* = 200)
(defglobal ?*min-obstacle-dist* = 100)
(defglobal ?*speed-update-step* = 5)


(defrule set-control-speed "Set the desired control speed"
?d<-(control on)
(not(control-speed ?))
=>
(retract ?d)
(printout t "FAIL! Set control speed in order to have automatic control!" crlf)
)

(defrule automatic-increase-speed "Increase speed"
  (control on) ; Automatic control is turned on
  (speed ?speed)
  (not(speed-limit ?sl&:(< ?sl (+ ?speed ?*speed-update-step*)))) ; If the increase of the speed will not cause to go over limit
  (not(obstacle ?od&:(< ?od ?*min-obstacle-dist*))) ; There is no obstacle or is far away
  (control-speed ?control-speed&:(> ?control-speed ?speed)) ; The current speed is below the desired speed
  (not (increment)) ; There is not an increment command already
  =>
  (assert (increment))
)

(defrule delete-redundant-increments "Deletes increment command if there is also a decrement command"
(declare (salience 1)) ;Salience has to be higher than speed-update-1
?incr<-(increment)
(decrement)
=>
(retract ?incr)
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-3
;;;;;;;;;;;;;;;;;;;;;;;

(defrule cruise-decrement-by-speed-limit "Decreases speed if it is over the limit"
  (declare (salience 4))
  (speed-limit ?sl)
  (speed ?s&:(> ?s ?sl))
  =>
  (assert (decrement))
  ; speed limit remains in the system
  (printout t "Speed limit " ?sl "! Breaking." crlf)
)

(deffunction intensity-by-distance (?d) "Determines the breaking intensity, based on the distance"
  (if (< ?d 30) then (bind ?result high)
  else (if (< ?d 60) then (bind ?result medium)
    else (if (< ?d 100) then (bind ?result low)
      else (bind ?result safe))))
  (return ?result)
)

(defrule decrement-because-obstacle "Decrease the speed because there is obstacle in front"
  (declare (salience 6))
  (control on)
  (obstacle ?d1&:(< ?d1 ?*min-obstacle-dist*)) ; it is closer than 100 m
  (speed ?s)
  =>
  (bind ?int (intensity-by-distance ?d1))
  (assert (breaks ?int))
  ; and it takes control to reach the corresponding cruise speed by means  of speed (increment)s.
  ; This happens in the automatic-increase-speed function.
  (printout t "Obstacle! Distance: " ?d1 "    Break Intensity: " ?int  crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-2
;;;;;;;;;;;;;;;;;;;;;;;

(defrule delete_farther_obstacles "Leave only the closest obstacle"
(declare (salience 10))
  (obstacle ?dist1)
  ?o <- (obstacle ?dist2)
  (test(< ?dist1 ?dist2)) ; dist1 is closer
=>
  (retract ?o)
  (printout t "(obstacle " ?dist2 ") deleted" crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-0
;;;;;;;;;;;;;;;;;;;;;;;

(defrule car-on
  ?on <- (car on)
  =>
  (retract ?on)
  (assert (control on))
  (assert (speed 0))
  (printout t "Car is started. Activating automatic control..." crlf)
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
    (case low then (bind ?result ?*speed-update-step*))
    (case medium then (bind ?result (* 2 ?*speed-update-step*)))
    (case high then (bind ?result (* 3 ?*speed-update-step*)))
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
  ?i <- (increment ?num&:(>= ?num ?*speed-update-step*))
  =>
  (assert (increment) (increment (- ?num ?*speed-update-step*)))
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
  ?i <- (decrement ?num&:(>= ?num ?*speed-update-step*))
  =>
  (assert (decrement) (decrement (- ?num ?*speed-update-step*)))
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
  ?incr  <- (increment)
  ?speed <- (speed ?s)
  =>
  (retract ?incr ?speed)
  (bind ?new_sp (min (+ ?s ?*speed-update-step*) ?*max-speed*))
  (assert (speed ?new_sp))
  (printout t "Speed increased to: " ?new_sp crlf)
)

(defrule speed-update-2
  (declare (salience 50))
  ?decr  <- (decrement)
  ?speed <- (speed ?s)
  =>
  (retract ?decr ?speed)
  (bind ?new_sp (max (- ?s ?*speed-update-step*) 0))
  (assert (speed ?new_sp))
  (printout t "Speed decreased to: " ?new_sp crlf)
)









