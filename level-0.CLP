;;;;;;;;;;;;;;;;;;;;;;;
;;;;level-0
;;;;;;;;;;;;;;;;;;;;;;;

;;FACTS
;;; (throttle low) (trhottle medium) (throttle high)
;;; (breaks low) (breaks medium) (breaks high)
;;; (car on) (car off) (speed x)
;;; (increment) (decrement)

(defglobal ?*max-speed* = 200)

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


;;TESTING
;;(assert (car on))
;;(run)
