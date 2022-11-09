;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname szeto-z-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;Zachary Szeto zdszeto

;Problem 1
(define-struct merchandise (name kind autographed? quantity price))
;; merchandise is a (make-merchandise String String Boolean Natural Number+)
;; interp:  represents a merchandise constrictor where
;;  name is the name of the various items sold
;;  kind is the type of merchandise (action figure, board game, costume, manga/comic book, trading card, etc)
;;  autographed? is whether or not the merchandise is autographed
;;  quantity is the number of items sold
;;  pricse is the price of a single item

;Examples
(define buzz-light-year (make-merchandise "Buzz Lightyear" "action figure" #false 1 50))
;; make-merchandise: String String Boolean Natural Natural -> Merchandise
;;  merchandise-name: merchandise -> String
;;  merchandise-kind: merchandise -> String
;;  merchandise-autographed?: merchandise -> Boolean
;;  merchandise-quantity: merchandise -> Natural
;;  merchandise-price: merchandise -> Natural

(define sorry! (make-merchandise "Sorry!" "board game" #false 500 10))
;; make-merchandise: String String Boolean Natural Natural -> Merchandise
;;  merchandise-name: merchandise -> String
;;  merchandise-kind: merchandise -> String
;;  merchandise-autographed?: merchandise -> Boolean
;;  merchandise-quantity: merchandise -> Natural
;;  merchandise-price: merchandise -> Natural

(define pokémon (make-merchandise "Pokémon" "trading card" #true 100 5))
;; make-merchandise: String String Boolean Natural Natural -> Merchandise
;;  merchandise-name: merchandise -> String
;;  merchandise-kind: merchandise -> String
;;  merchandise-autographed?: merchandise -> Boolean
;;  merchandise-quantity: merchandise -> Natural
;;  merchandise-price: merchandise -> Natural

;Problem 2
;merchandise Template
;merchandise.fcn: merchandise...->...
;(define (make-merchandise a-merchandise...))
;  (merchandise-name: a-merchandise...)
;  (merchandise-kind: a-merchandise...)
;  (merchandise-autographed?: a-merchandise...)
;  (merchandise-quantity: a-merchandise...)
;  (merchandise-price: a-merchandise...)

;Problem 3
(define Receipt-1 (cons sorry! (cons buzz-light-year empty)))
(define Receipt-2 (cons (make-merchandise "Clue" "board game" #false 250 15) (cons pokémon (cons (make-merchandise "Batman" "comic" #true 750 12)  empty))))

;Problem 4
; ;; Receipt-fcn:  ListOfMerchandise ->
; ;;
; (define (Receipt-fcn alom)
;   (cond [(empty? alom)  (...) ]
;         [(cons? alom)   (...        (Receipt-fcn (first aloc))
;                                     (Receipt-fcn (rest aloc)))]))

;Problem 5
(define Receipt-3 (list));empty list used for testing

;;Signature: Receipt Number+ -> Boolean
;;Purpose: consumes a receipt and a number (representing a threshold cost) and returns true if the first item in that receipt is autographed and less than the threshold cost
(define (cheap-autograph? alom a-num)
  (and (boolean=? (merchandise-autographed? (first alom)) #true) (<  (merchandise-price (first alom)) a-num)))

(check-expect (cheap-autograph? (list (make-merchandise "Clue" "board game" #true 250 15)) 15) #false);price is equal to threshold and is autographed
(check-expect (cheap-autograph? (list (make-merchandise "Clue" "board game" #true 250 15)) 14) #false);price is greater than threshold and is autographed
(check-expect (cheap-autograph? (list (make-merchandise "Clue" "board game" #true 250 15)) 16) #true);price is less than threshold and is autographed
(check-expect (cheap-autograph? (list (make-merchandise "Clue" "board game" #false 250 15)) 15) #false);price is equal to threshold and is not autographed
(check-expect (cheap-autograph? (list (make-merchandise "Clue" "board game" #false 250 15)) 14) #false);price is greater than threshold and is not autographed
(check-expect (cheap-autograph? (list (make-merchandise "Clue" "board game" #false 250 15)) 16) #false);price is less than threshold and is not autographed

;;Signature: Receipt Number+ -> Receipt
;;Purpose: consumes a receipt and a number (representing a threshold cost) and produces a receipt
(define (list-cheap-autograph alom a-num);alom = a list of merchandise = receipt
  (cond [(empty? alom) empty]
        [(cons? alom) (if (cheap-autograph? alom a-num)
                          (cons (first alom) (list-cheap-autograph (rest alom) a-num))
                          (list-cheap-autograph (rest alom) a-num))]))

(check-expect (list-cheap-autograph Receipt-3 0) empty) ; 0 threshold cost, empty list
(check-expect (list-cheap-autograph Receipt-3 100000) empty) ; 100000 threshold cost, empty list
(check-expect (list-cheap-autograph Receipt-2 0) empty) ; 0 threshold cost, populated list
(check-expect (list-cheap-autograph Receipt-2 1000) (list (make-merchandise "Pokémon" "trading card" #true 100 5)(make-merchandise "Batman" "comic" #true 750 12))) ; 1000 threshold cost, populated list
(check-expect (list-cheap-autograph Receipt-2 100) (list (make-merchandise "Pokémon" "trading card" #true 100 5)(make-merchandise "Batman" "comic" #true 750 12))) ; 100 threshold cost, populated list

;Problem 6
;;Signature: Receipt -> Boolean
;;Purpose: consumes a receipt and produces true if the first item in that receipt is a trading card
(define (trading-cards? alom)
  (string=? (merchandise-kind (first alom)) "trading card"))

(check-expect (trading-cards? (list (make-merchandise "Clue" "board game" #true 250 15))) #false); is not a trading card
(check-expect (trading-cards? (list (make-merchandise "Pokémon" "trading card" #true 100 5))) #true); is a trading card

;;Signature: Receipt -> Natural
;;Purpose: consumes a receipt and returns the total number of items in the order that are trading cards
(define (count-trading-cards alom)
  (cond [(empty? alom) 0]
        [(cons? alom) (if (trading-cards? alom)
                          (+ (merchandise-quantity (first alom)) (count-trading-cards (rest alom)))
                          (count-trading-cards (rest alom)))]))

(check-expect (count-trading-cards Receipt-1) 0) ; list with no trading cards but other toy
(check-expect (count-trading-cards Receipt-2) 100) ; list with trading cards and non-trading cards
(check-expect (count-trading-cards Receipt-3) 0) ; empty list, no trading cards
(check-expect (count-trading-cards (list (make-merchandise "Pokémon" "trading card" #false 0 5)(make-merchandise "Digimon" "trading card" #false 0 12))) 0) ; list with trading cards but quantity 0
(check-expect (count-trading-cards (list (make-merchandise "Pokémon" "trading card" #false 10 5)(make-merchandise "Digimon" "trading card" #false 10 12))) 20) ; list with multiple different trading cards
(check-expect (count-trading-cards (list (make-merchandise "Pokémon" "trading card" #false 0 5)(make-merchandise "Digimon" "trading card" #false 10 12))) 10) ; list with multiple different trading cards, one w/ 0 and one with 10


;Problem 7
;;Signature: Receipt -> Number+
;;Purpose:consumes a receipt and produces the total value of the first item in that receipt (quantity * price)
(define (value-merchandise alom)
  (* (merchandise-price (first alom)) (merchandise-quantity (first alom))))

(check-expect (value-merchandise (list(make-merchandise "Pokémon" "trading card" #false 0 0))) 0);1 item 0 quantity 0 price
(check-expect (value-merchandise (list(make-merchandise "Pokémon" "trading card" #false 100 0))) 0);1 item 100 quantity 0 price
(check-expect (value-merchandise (list(make-merchandise "Pokémon" "trading card" #false 0 100))) 0);1 item 0 quantity 100 price
(check-expect (value-merchandise (list(make-merchandise "Pokémon" "trading card" #false 100 100))) 10000);1 item 100 quantity 100 price
        
;;Signature: Receipt -> Number+
;;Purpose: consumes a receipt and produces the total cost of all the merchandise items (a number)
(define (receipt-total alom)
  (cond [(empty? alom) 0]
        [(cons? alom) (+ (value-merchandise alom) (receipt-total (rest alom)))]))

(check-expect (receipt-total Receipt-3) 0);empty list
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 0 0))) 0);1 item 0 quantity 0 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 100 0))) 0);1 item 100 quantity 0 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 0 100))) 0);1 item 0 quantity 100 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 100 100))) 10000);1 item 100 quantity 100 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 0 0)(make-merchandise "Digimon" "trading card" #false 0 0))) 0);2 items both 0 quantity 0 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 0 0)(make-merchandise "Digimon" "trading card" #false 100 0))) 0);2 items with one 0 quantity 0 price and the other 100 quantity 0 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 0 0)(make-merchandise "Digimon" "trading card" #false 0 100))) 0);2 items with one 0 quantity 0 price and the other 0 quantity 100 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 100 0)(make-merchandise "Digimon" "trading card" #false 0 0))) 0);2 items with one 100 quantity 0 price and the other 0 quantity 0 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 0 100)(make-merchandise "Digimon" "trading card" #false 0 0))) 0);2 items with one 0 quantity 100 price and the other 100 quantity 0 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 100 100)(make-merchandise "Digimon" "trading card" #false 100 100))) 20000);2 items both 100 quantity 100 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 100 100)(make-merchandise "Digimon" "trading card" #false 100 0))) 10000);2 items with one 100 quantity 100 price and the other 100 quantity 0 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 100 100)(make-merchandise "Digimon" "trading card" #false 0 100))) 10000);2 items with one 100 quantity 100 price and the other 0 quantity 100 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 100 0)(make-merchandise "Digimon" "trading card" #false 100 100))) 10000);2 items with one 100 quantity 0 price and the other 100 quantity 100 price
(check-expect (receipt-total (list(make-merchandise "Pokémon" "trading card" #false 0 100)(make-merchandise "Digimon" "trading card" #false 100 100))) 10000);2 items with one 0 quantity 100 price and the other 100 quantity 100 price

;Problem 8
;;Signature: Receipt -> Boolean
;;Purpose: consumes a receipt and returns true if the first item in that receipt is a board game
(define (board-game? alom)
  (string=? (merchandise-kind (first alom)) "board game"))

(check-expect (board-game? (list (make-merchandise "Chess" "board game" #false 35 15))) #true) ;board game
(check-expect (board-game? (list (make-merchandise "Fireman" "costume" #false 10 10))) #false) ;not a board game

;;Signature: Receipt -> Number+
;;Purpose: consumes a receipt and produces the total cost of all the board games contained in the receip
(define (board-games-cost alom)
  (cond [(empty? alom) 0]
        [(cons? alom) (if (board-game? alom)
                          (+ (value-merchandise alom) (board-games-cost (rest alom)))
                          (board-games-cost (rest alom)))]))

(check-expect (board-games-cost Receipt-3) 0);empty list
(check-expect (board-games-cost (list (make-merchandise "Chess" "board game" #false 35 15) sorry! (make-merchandise "Fireman" "costume" #false 10 10))) 5525); two board game and one non-board game
(check-expect (board-games-cost (list (make-merchandise "Chess" "board game" #false 35 15)  (make-merchandise "Fireman" "costume" #false 10 10))) 525); one board game and one non-board game
(check-expect (board-games-cost (list (make-merchandise "Chess" "board game" #false 35 15))) 525);one board game
(check-expect (board-games-cost (list (make-merchandise "Fireman" "costume" #false 10 10))) 0);one non-board game

              
;Problem 9
;;Signature: Receipt Decimal+ -> Number+
;;Purpose: consumes a receipt and a positive decimal and produces the total discounted value of the first item in that receipt if it is a costume (quantity * price * discount)
(define (discount-value-merchandise alom discount)
  (* (merchandise-price (first alom)) (merchandise-quantity (first alom)) (- 1 discount)))
  
(check-expect (discount-value-merchandise (cons (make-merchandise "Fireman" "costume" #false 1 100)(cons (make-merchandise "Superman" "comic" #true 1 150)  empty)) 0.50) 50) ; Discount on only first item in list
(check-expect (discount-value-merchandise (cons (make-merchandise "Fireman" "costume" #false 1 50)  empty) 0.50) 25) ; Discount on only one item in list
(check-expect (discount-value-merchandise (cons (make-merchandise "Fireman" "costume" #false 1 50)  empty) 0) 50) ; 0% Discount
(check-expect (discount-value-merchandise (cons (make-merchandise "Fireman" "costume" #false 1 50)  empty) 1) 0) ; 100% Discount
(check-expect (discount-value-merchandise (cons (make-merchandise "Fireman" "costume" #false 1 0)  empty) 0.50) 0) ; Discounts 0 cost item

;;Signature: Receipt Number+ -> Number+
;;Purpose: consumes a receipt and a number representing the discount on costume items (in decimal form) and produces the total cost of the receipt, with the discount applied only to costume merchandise
(define (halloween-sale alom discount)
  (cond [(empty? alom) 0]
        [(cons? alom) (if (string=? (merchandise-kind (first alom)) "costume")
                          (+  (discount-value-merchandise alom discount)(halloween-sale (rest alom) discount))
                          (+ (value-merchandise alom) (halloween-sale (rest alom) discount)))]))
  
(check-expect (halloween-sale (cons (make-merchandise "Fireman" "costume" #false 10 10) (cons pokémon (cons (make-merchandise "Superman" "comic" #true 1 150)  empty))) 0.25) 725) ; Only discounts costume
(check-expect (halloween-sale (cons (make-merchandise "Fireman" "costume" #false 10 10) (cons pokémon (cons (make-merchandise "Superman" "comic" #true 1 150)  empty))) 0.50) 700) ; Larger discount on costume
(check-expect (halloween-sale (cons (make-merchandise "Fireman" "costume" #false 1 50)(cons (make-merchandise "Waterman" "costume" #true 1 50)  empty)) 0.25) 75) ; Discounts multiple costumes
(check-expect (halloween-sale (cons (make-merchandise "Not Costume" "comic" #false 10 10) (cons pokémon (cons (make-merchandise "Superman" "comic" #true 1 150)  empty))) 0.25) 750) ; No discount applied
(check-expect (halloween-sale (cons (make-merchandise "Fireman" "costume" #false 10 10) (cons pokémon (cons (make-merchandise "Superman" "comic" #true 1 150)  empty))) 0) 750) ; 0% off discount on costume
(check-expect (halloween-sale (cons (make-merchandise "Fireman" "costume" #false 10 10) (cons pokémon (cons (make-merchandise "Superman" "comic" #true 1 150)  empty))) 1) 650) ; 100% off discount on costume
(check-expect (halloween-sale Receipt-3 0.25) 0) ; Discounts empty list, 0 cost
