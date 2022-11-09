;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname szeto-z-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;Zachary Szeto zdszeto

;Problem 1
(define-struct tornado (scale distance max-winds))
;; a Tornado is a (make-tornado String Natural Natural)
;; interp:  represents a tornado constrictor where
;;  scale is the tornado's Fujita scale ("F0", "F1", "F2", ... "F5")
;;  distance is its distance traveled (in miles)
;;  max-winds is the tornado's maximum winds (in miles per hour)

(define-struct hurricane (name category max-winds velocity heading))
;; a Hurricane is a (make-hurricane String Natural Natural Natural String)
;; interp:  represents a hurricane constrictor where
;;  name is the hurricane's name
;;  category is its category (a number between 1 and 5)
;;  max-winds is the hurricane's maximum sustained winds (in miles per hour)
;;  velocity is the velocity of the hurricane (in miles per hour)
;;  heading is the hurricane's heading (for example, "NNW")

(define-struct thunderstorm (heading velocity rainfall max-winds))
;; a Thunderstorm is a (make-thunderstorm String Natural Natural Natural)
;; interp:  represents a thunderstorm constrictor where
;;  heading is the thunderstorm's heading (for example, "NNW")
;;  velocity is the velocity of the thunderstorm (in miles per hour)
;;  rainfall is the thunderstorm's total rainfall (in inches)
;;  max-winds is the thunderstorm's maximum wind gusts (in miles per hour)

;Data Definition
;windstorm is one of:
; tornado
; hurricane
; thunderstorm

;Examples
(define jeff (make-tornado "F3" 142 201))
;; make-tornado: String Natural Natural -> Tornado
;;  tornado-scale: Tornado -> String
;;  tornado-distance: Tornado -> Natural
;;  tornado-max-winds: Tornado -> Natural

(define bob (make-hurricane "Sandy" 3 110 28 "WNW"))
;; make-hurricane: String Natural Natural Natural Natural String -> Hurricane
;;  hurricane-name: Hurricane -> String
;;  hurricane-category: Hurricane -> Natural
;;  hurricane-max-winds: Hurricane -> Natural
;;  hurricane-velocity: Hurricane -> Natural
;;  hurricane-heading: Hurricane -> String

(define hubert (make-thunderstorm "SSW" 19 9 71))
;; make-thunderstorm: String Natural Natural -> Thunderstorm
;;  thunderstorm-heading: Thunderstorm -> String
;;  thunderstorm-velocity: Thunderstorm -> Natural
;;  thunderstorm-rainfall: Thunderstorm -> Natural
;;  thunderstorm-max-winds: Thunderstorm -> Natural

;Problem 2

;Tornado Template
;tornado.fcn: tornado...->...
;(define (make-tornado a-tornado...))
;  (tornado-scale a-tornado...)
;  (tornado-distance a-tornado...)
;  (tornado-max-winds a-tornado..)

;Hurricane Template
;hurricane.fcn: hurricane...->...
;(define (make-hurricane a-hurricane...))
;  (hurricane-name a-hurricane...)
;  (hurricane-category a-hurricane...)
;  (hurricane-max-winds a-hurricane...)
;  (hurricane-velocity a-hurricane...)
;  (hurricane-heading a-hurricane...)

;Thunderstorm Template
;thunderstorm.fcn: thunderstorm...->...
;(define (make-thunderstorm a-thunderstorm...))
;  (thunderstorm-heading a-thunderstorm...)
;  (thunderstorm-velocity a-thunderstorm...)
;  (thunderstorm-rainfall a-thunderstorm...)
;  (thunderstorm-max-winds a-thunderstorm...)

;Windstorm Template
;(define (windstorm a-windsotrm))(
;  cond [(tornado? a-windstorm) tornado.fcn]
;        [(hurricane? a-windstorm) hurricane.fcn]
;        [(thunderstorm? a-windstorm) thunderstorm.fcn]
;))

;Question 4
;;Signature: Windstorm -> Boolean
;;Purpose: Consumes a windstorm and returns true if the windstorm is a tornado with a Fujita scale rating of "F4" or "F5", a category 4 or 5 hurricane, or a thunderstorm with more than 5 inches of rainfall and winds exceeding 50 mph.
(define (violent? a-windstorm)
  (cond [(and (tornado? a-windstorm) (or (string=?(tornado-scale a-windstorm) "F4") (string=?(tornado-scale a-windstorm) "F5")))#true]
        [(and (hurricane? a-windstorm) (or (= (hurricane-category a-windstorm) 4) (= (hurricane-category a-windstorm) 5))) #true]
        [(and (thunderstorm? a-windstorm)(and (> (thunderstorm-rainfall a-windstorm) 5)(> (thunderstorm-max-winds a-windstorm) 50))) #true]
        [else #false]))

(check-expect (violent? (make-tornado "F0" 100 10)) #false) ;Tornado with a Fujita scale of F0
(check-expect (violent? (make-tornado "F1" 100 10)) #false) ;Tornado with a Fujita scale of F1
(check-expect (violent? (make-tornado "F2" 100 10)) #false) ;Tornado with a Fujita scale of F2
(check-expect (violent? (make-tornado "F3" 100 10)) #false) ;Tornado with a Fujita scale of F3
(check-expect (violent? (make-tornado "F4" 100 10)) #true) ;Tornado with a Fujita scale of F4
(check-expect (violent? (make-tornado "F5" 100 10)) #true) ;Tornado with a Fujita scale of F5
(check-expect (violent? (make-tornado "F6" 100 10)) #false) ;Tornado with a Fujita scale of F6

(check-expect (violent? (make-hurricane "NAME" 6 10 10 "N")) #false) ;Category 6 hurricane
(check-expect (violent? (make-hurricane "NAME" 5 10 10 "N")) #true) ;Category 5 hurricane
(check-expect (violent? (make-hurricane "NAME" 4 10 10 "N")) #true) ;Category 4 hurricane
(check-expect (violent? (make-hurricane "NAME" 3 10 10 "N")) #false) ;Category 3 hurricane
(check-expect (violent? (make-hurricane "NAME" 2 10 10 "N")) #false) ;Category 2 hurricane
(check-expect (violent? (make-hurricane "NAME" 1 10 10 "N")) #false) ;Category 1 hurricane
(check-expect (violent? (make-hurricane "NAME" 0 10 10 "N")) #false) ;Category 0 hurricane

(check-expect (violent? (make-thunderstorm "N" 0 6 51)) #true) ;Thunderstorm with 6 inches of rainfall and winds of 51 mph
(check-expect (violent? (make-thunderstorm "N" 0 7 70)) #true) ;Thunderstorm with 7 inches of rainfall and winds of 70 mph
(check-expect (violent? (make-thunderstorm "N" 0 5 50)) #false) ;Thunderstorm with 5 inches of rainfalll and winds of 50 mph
(check-expect (violent? (make-thunderstorm "N" 0 0 0)) #false) ;Thunderstorm with 0 of inches of rainfall and winds of 0 mph
(check-expect (violent? (make-thunderstorm "N" 0 6 0)) #false) ;Thunderstorm with 6 inches of rainfall and winds of 0 mph
(check-expect (violent? (make-thunderstorm "N" 0 0 51)) #false) ;Thunderstorm with 0 inches of rainfall and winds of 0 mph

;Question 5
;;Signature: Windstorm Natural -> Windstorm
;;Purpose: Consumes a windstorm and a number representing wind speeds and produces a windstorm. The windstorm that's produced is a windstorm the same as the original, except that its max-winds is changed to the new wind speeds. 
(define (change-max-winds a-windstorm new-max-winds)
  (cond [(tornado? a-windstorm) (make-tornado (tornado-scale a-windstorm) (tornado-distance a-windstorm) new-max-winds)]
        [(hurricane? a-windstorm) (make-hurricane (hurricane-name a-windstorm) (hurricane-category a-windstorm) new-max-winds (hurricane-velocity a-windstorm) (hurricane-heading a-windstorm))]
        [(thunderstorm? a-windstorm) (make-thunderstorm (thunderstorm-heading a-windstorm) (thunderstorm-velocity a-windstorm) (thunderstorm-velocity a-windstorm) new-max-winds)]))

(check-expect (change-max-winds(make-tornado "F1" 0 0) 100) (make-tornado "F1" 0 100)) ;Changes from 0 to 100 mph
(check-expect (change-max-winds(make-tornado "F1" 0 100) 0) (make-tornado "F1" 0 0)) ;Changes from 100 to 0 mph
(check-expect (change-max-winds(make-tornado "F1" 0 0) 0) (make-tornado "F1" 0 0)) ;Changes from 0 to 0 mph
(check-expect (change-max-winds(make-tornado "F1" 0 100) 100) (make-tornado "F1" 0 100)) ;Changes from 100 to 100 mph

(check-expect (change-max-winds(make-hurricane "Name" 0 0 0 "N") 100) (make-hurricane "Name" 0 100 0 "N")) ;Changes from 0 to 100 mph
(check-expect (change-max-winds(make-hurricane "Name" 0 100 0 "N") 0) (make-hurricane "Name" 0 0 0 "N")) ;Changes from 100 to 0 mph
(check-expect (change-max-winds(make-hurricane "Name" 0 0 0 "N") 0) (make-hurricane "Name" 0 0 0 "N")) ;Changes from 0 to 0 mph
(check-expect (change-max-winds(make-hurricane "Name" 0 100 0 "N") 100) (make-hurricane "Name" 0 100 0 "N")) ;Changes from 100 to 100 mph

(check-expect (change-max-winds(make-thunderstorm "N" 0 0 0) 100) (make-thunderstorm "N" 0 0 100)) ;Changes from 0 to 100 mph
(check-expect (change-max-winds(make-thunderstorm "N" 0 0 100) 0) (make-thunderstorm "N" 0 0 0)) ;Changes from 100 to 0 mph
(check-expect (change-max-winds(make-thunderstorm "N" 0 0 0) 0) (make-thunderstorm "N" 0 0 0)) ;Changes from 0 to 0 mph
(check-expect (change-max-winds(make-thunderstorm "N" 0 0 100) 100) (make-thunderstorm "N" 0 0 100)) ;Changes from 100 to 100 mph


;Question 6
;;Signature: ListOfString -> String
;;Purpose: consumes a ListOfString and produces a String. The function produces a string consisting of just the first character of each string in the ListOfString.   
(define (acrostic alos)
  (cond[(empty? alos) ""]
       [(cons? alos) (string-append (substring (first alos) 0 1) (acrostic (rest alos)))]))

(check-expect (acrostic (cons "Zebra" (cons "Anteater" (cons "Cheetah" (cons "Hippo" empty))))) "ZACH") ;List with Zebra, Anteater, Cheetah, and Hippo
(check-expect (acrostic (cons "Ibex" (cons "Antelope" (cons "Night owl" empty)))) "IAN"); List with Ibex, Antelope, and Night Owl
(check-expect (acrostic empty) "") ; Empty List
(check-expect (acrostic (cons "The best food on campus is halal shack" empty)) "T") ; List of "The best food on campus is halal shack"



;Question 7
;;Signature: ListOfString -> ListOfString
;;Purpose: Consumes a ListOfString and produces a ListOfString. The list that's produced contains only those strings from the original list that have "ickle" as a substring somewhere in them.
(define (ickle-strings alos)
  (cond [(empty? alos) empty]
        [(cons? alos) (if (string-contains-ci? "ickle" (first alos))
                          (cons (first alos) (ickle-strings (rest alos)))
                          (ickle-strings (rest alos)))]))

(check-expect (ickle-strings (cons "pickle" (cons "henry" (cons "sickle" (cons "fickle" (cons "burger" empty)))))) (cons "pickle" (cons "sickle" (cons "fickle" empty)))) ; List with pickle, henry, sickle, fickle, burger
(check-expect (ickle-strings (cons "tom" (cons "henry" (cons "john" (cons "bob" (cons "burger" empty)))))) empty) ; List with tom, henry, john, bob, burger
(check-expect (ickle-strings empty) empty) ; Empty list
(check-expect (ickle-strings (cons "pICKLE" (cons "fickle" (cons "sIcKle" (cons "nickLE" empty))))) (cons "pICKLE" (cons "fickle" (cons "sIcKle" (cons "nickLE" empty))))) ; List with pickle, fickle, sickle, nickle but with different cases
(check-expect (ickle-strings (cons "pickle" (cons "bob" empty))) (cons "pickle" empty)) ; List with pickle, bob


;Question 8
;;Data Definition
;a ListOfNaturals is one of...
;empty
;(cons Natural ListOfNatural)
;interp. ListOfNaturals represents a list of numbers (ie. 1, 2, 3...)

;;Signature: ListOfString -> ListOfNatural
;;Purpose: Consumes a ListOfString and produces a ListOfNatural.
(define (lengths-of-strings alos)
  (cond [(empty? alos) empty]
        [(cons? alos) (cons (string-length(first alos)) (lengths-of-strings (rest alos)))]))

(check-expect (lengths-of-strings (cons "a" (cons "ab" (cons "abc" (cons "abcd" (cons "abcde" empty)))))) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))) ; List with a, ab, abc, etc.
(check-expect (lengths-of-strings (cons "     " (cons "    " (cons "   " (cons "  " (cons " " empty)))))) (cons 5 (cons 4 (cons 3 (cons 2 (cons 1 empty)))))) ; List with only spaces
(check-expect (lengths-of-strings (cons "123abc" (cons "4.44" (cons "0" empty)))) (cons 6 (cons 4 (cons 1 empty)))) ; List with decimal, 0, etc.
(check-expect (lengths-of-strings (cons "" empty)) (cons 0 empty)) ; List with string length 0
(check-expect (lengths-of-strings empty) empty) ; Empty list
