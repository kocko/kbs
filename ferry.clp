;===== ask questions functions

(deffunction ask-question (?question $?allowed-values)
    (printout t crlf)
    (printout t ?question)
    (printout t crlf)
    (bind ?answer (read))
    (if (lexemep ?answer) then
        (bind ?answer (lowcase ?answer)))
    (while (not (member ?answer ?allowed-values)) do
        (printout t crlf)
        (printout t ?question)
        (bind ?answer (read))
        (if (lexemep ?answer) then
            (bind ?answer (lowcase ?answer))))
    ?answer)
   
(deffunction ask-question-number-ans (?question)
    (printout t crlf)
    (printout t ?question)
    (printout t crlf)
    (bind ?answer (read))
    (while (not (numberp ?answer)) do
        (printout t crlf)
        (printout t ?question)
        (bind ?answer (read)))
    ?answer)

(deffunction ask-question-integer-ans (?question)
    (printout t crlf)
    (printout t ?question)
    (printout t crlf)
    (bind ?answer (read))
    (while (not (integerp ?answer)) do
        (printout t crlf)
        (printout t ?question)
        (bind ?answer (read)))
    ?answer)

(deffunction ask-question-integer-ans-in-bounds (?question ?lower-bound ?upper-bound)
    (bind ?answer
          (ask-question-integer-ans ?question))
    (while (not (<= ?lower-bound ?answer ?upper-bound))
        (bind ?answer
              (ask-question-integer-ans ?question)))
    ?answer)

(deffunction yes-or-no-p (?question)
    (bind ?response (ask-question ?question yes no y n))
    (if (or (eq ?response yes) (eq ?response y)) then
        TRUE
    else
        FALSE))

;===== templates

(deftemplate ferry
	(slot type)
  (slot from)
  (slot to)
	(slot departure-time)
	(slot initial-price)
  (slot children-per-person)
	(slot animals-allowed)
  (slot restaurant)
  (slot cars)
  (slot vans)
  (slot trucks)
  (slot rooms)
)

;===== initial-facts

(deffacts initial 
  (proceed)
  (ferry
    (type catamaran)
    (from dover)
    (to calais)
    (departure-time 9)
    (initial-price 15)
    (animals-allowed yes)
    (children-per-person 2)
    (restaurant yes)
    (cars 50)
    (vans 20)
    (trucks 20)
    (rooms 100)
  )
  (ferry
    (type ferry)
    (from liverpool)
    (to dublin)
    (departure-time 11)
    (initial-price 30)
    (animals-allowed yes)
    (children-per-person 1)
    (restaurant no)
    (cars 50)
    (vans 20)
  )
  (ferry
    (type car-ferry)
    (from hull)
    (to rotterdam)
    (departure-time 13)
    (initial-price 20)
    (children-per-person 4)
    (animals-allowed no)
    (restaurant no)
    (cars 100)
  )
)


;===== TODO: add more facts

;===== check for facts rules
(defrule check-from "From : Dover, Hull, Liverpool"
    =>
    (bind ?response (ask-question "Where do you depart from? (Dover, Hull, Liverpool)" dover hull liverpool Dover Hull Liverpool))
    (if (or (eq ?response dover) (eq ?response Dover)) then
        (assert (from dover))
    else
        (if (or (eq ?response hull) (eq ?response Hull)) then
            (assert (from hull))
        else
            (assert (from liverpool))))
    (assert (checked from)))


(defrule check-to "To : Calais, Rotterdam, Dublin"
    =>
    (bind ?response (ask-question "Where do you arrive in? (Calais, Rotterdam, Dublin)" calais rotterdam dublin Calais Rotterdam Dublin))
    (if (or (eq ?response calais) (eq ?response Calais)) then
        (assert (to calais))
    else
        (if (or (eq ?response Rotterdam) (eq ?response rotterdam)) then
            (assert (to rotterdam))
        else
            (assert (to dublin))))
    (assert (checked to)))

(defrule check-deparature-not-before "After: hour"
    =>
    (bind ?response (ask-question-number-ans "Earliest hour of departure? (between 6 and 20. Type 0 if it does not matter)"))
    (if (and (>= ?response 6) (<= ?response 20)) 
      then (assert (after ?response)))
    (assert (checked after)))

(defrule check-deparature-not-after "Before: hour"
    =>
    (bind ?response (ask-question-number-ans "Latest hour of departure? (between 6 and 20. Type 0 if it does not matter)"))
    (if (and (>= ?response 6) (<= ?response 20)) 
      then (assert (before ?response)))
    (assert (checked before)))

(defrule check-adults "Adults count"
    =>
    (bind ?response (ask-question-integer-ans-in-bounds "Number of adults? (between 1 and 5)" 1 5))
    (assert (adults ?response))
    (assert (checked adults)))

(defrule check-children "Children count"
    =>
    (bind ?response (ask-question-number-ans "Number of children?"))
    (assert (children ?response))
    (assert (checked children)))

(defrule check-vehicle-type "Vehicle type: car, van or truck"
    =>
    (bind ?response (ask-question "What is your vehicle type? (car, van or truck)" car Car van Van truck Truck))
    (assert (vehicle ?response))
    (assert (checked vehicle)))

(defrule check-animals "Animals: yes/no"
    =>
    (if (yes-or-no-p "Do you bring animals with you? (yes/no)") then
        (assert (animals yes)))
    (assert (checked animals)))

(defrule check-max-price "Maximum price"
    =>
    (bind ?response (ask-question-number-ans "Maximum price? (between 1 and 10000. Type 0 if it does not matter)"))
    (if (and (>= ?response 1) (<= ?response 10000)) 
      then (assert (max-price ?response)))
    (assert (checked max-price)))

(defrule check-cheapest-or-earliest "Cheapest or earliest ferry wanted"
    =>
    (bind ?response (ask-question "Do you want the cheapest or the earliest ferry we have?" cheapest c earliest e))
    (if (or (eq ?response c) (eq ?response cheapest)) then
        (assert (cheapest))
    else
       (assert (earliest)))
    (assert (checked cheapest-or-earliest)))
	
(defrule check-room-need "Room needed: yes/no"
    =>
    (if (yes-or-no-p "Do you need a room on the ferry? (yes/no)") then
        (assert (room-needed)))
    (assert (checked room-needed)))
	
(defrule proceed-with-another-reservation "Proceed: yes/no"
	(declare (salience -1))
  ?from <- (from ?)
  ?checked-from <- (checked from)
  ?to <- (to ?)
  ?checked-to <- (checked to)
	=>
	(if (yes-or-no-p "Do you want to proceed? (yes/no)") then
    (retract ?from)
    (retract ?checked-from)
    (retract ?to)
    (retract ?checked-to)
		(refresh check-from)
		(refresh check-to)
		(refresh check-deparature-not-after)
		(refresh check-deparature-not-before)
		(refresh check-adults)
		(refresh check-children)
		(refresh check-vehicle-type)
		(refresh check-animals)
		(refresh check-cheapest-or-earliest)
		(refresh check-max-price)
		(refresh check-room-need)
    (refresh proceed-with-another-reservation)))

(defrule all-checked "Assert that all characteristics were checked"
    (checked from)
    (checked to)
    (checked after)
    (checked before)
    (checked adults)
    (checked children)
    (checked vehicle)
    (checked animals)
    (checked max-price)
    (checked cheapest-or-earliest)
    (checked room-needed)
    =>
    (printout t crlf)
    (assert (all-checked)))
	
;===== validation rules

(defrule validate-children-per-person
	(all-checked)
	(children ?children)
	(adults ?adults)
	=>
	(if (> ?children ?adults) then
		(printout t "There are no ferries which allow more than one child per an adult." crlf)
		(assert (error))))
		
(defrule validate-time-span
	(all-checked)
	(before ?before)
	(after ?after)
	=>
	(if (> ?after ?before) then
		(printout t "The time-span between the after and before departure time should be closed." crlf)
		(assert (error))))
		

;===== evaluation rules
