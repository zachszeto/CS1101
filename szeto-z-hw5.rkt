;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname szeto-z-hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;Zachary Szeto zdszeto

;; Part 1: Hierarchies

;; 1. Data definition(s)
(define-struct river (name ph bloom? tributaries))
;; a river is a (make-river String Natural Boolean ListOfRiver)
;; interp: represents a river constrictor where
;;  name is the name of the river
;;  ph is the ph level of the water (0-14)
;;  bloom? returns true if algal blooms are present otherwise it returns false
;;  tributaries is a list of rivers that feed into the river

;;ListOfRiver is one of...
;;empty
;;(cons river ListOfRiver)

;; 2. Example
(define amazon-river (make-river "Amazon" 7 #false ;gen 0
                                 (list (make-river "Mississippi" 9 #true ;gen 1
                                                   (list (make-river "Euphrates" 10 #false ;gen 2
                                                                     (list))));gen 3
                                       (make-river "Nile" 2 #false ;gen 1
                                                   (list (make-river "Yangtze" 5 #false ;gen 2
                                                                     (list));gen 3
                                                         (make-river "Congo" 7 #false ;gen 2
                                                                     (list))));gen 3
                                       (make-river "Tigris" 8 #false ;gen 1
                                                   (list (make-river "Volga" 9.5 #true ;gen 2
                                                                     (list)))))));gen 3

(define nile-river (make-river "Nile" 2 #false ;gen 0
                               (list (make-river "Yangtze" 5 #false ;gen 1
                                                 (list));gen 2
                                     (make-river "Congo" 7 #false ;gen 1
                                                 (list)))));gen 2

(define volga-river (make-river "Volga" 9.5 #true ;gen 0
                                (list))) ;gen 1



(define empty-river (make-river "" 0 #false (list)))

;; 3. Templates
;;;template for functions on River
;#;(define (fcn-for-River   a-river)
;  (... (river-name         a-river)
;       (river-ph           a-river)
;       (river-bloom?       a-river)
;       (river-tributaries  a-river)))

; ;; lor-fcn:  ListOfRiver ->
; ;;
; (define (tributaries-fcn: alor)
;   (cond [(empty? alor)  (...) ]
;         [(cons? alor)   (...        (los-fcn (first alor))
;                                     (los-fcn (rest alor)))]))

;; 4. list-alkaline-rivers
;;list-alkaline-rivers River -> ListOfRiver
;;consumes a river system and produces a list of the names of rivers in the system that have a pH level of 9.5 or greater
(define (list-alkaline-rivers a-river)
  (if (>= (river-ph a-river) 9.5)
      (cons (river-name a-river) (list-alkaline-rivers-helper (river-tributaries a-river)))
      (list-alkaline-rivers-helper (river-tributaries a-river))))

(check-expect (list-alkaline-rivers amazon-river) (list "Euphrates" "Volga"));checks amazon river
(check-expect (list-alkaline-rivers empty-river) (list));checks empty river
(check-expect (list-alkaline-rivers nile-river) (list));checks nile river
(check-expect (list-alkaline-rivers volga-river) (list "Volga"));checks volga river

;;list-alkaline-rivers-helper River ListOfRiver -> ListOfRiver
;;consumes a list of rivers and produces a list of the names of rivers in the system that have a pH level of 9.5 or greater
(define (list-alkaline-rivers-helper alor)
  (cond [(empty? alor) empty]
        [(cons? alor) (append  (list-alkaline-rivers (first alor))
                               (list-alkaline-rivers-helper (rest alor)))]))

(check-expect (list-alkaline-rivers-helper (river-tributaries amazon-river)) (list "Euphrates" "Volga"));checks amazon tributaries
(check-expect (list-alkaline-rivers-helper (river-tributaries empty-river)) (list));checks empty tributaries
(check-expect (list-alkaline-rivers-helper (river-tributaries nile-river)) (list));checks nile tributaries
(check-expect (list-alkaline-rivers-helper (river-tributaries volga-river)) (list));checks volga tributaries


;; 5. algae-free?
;;algae-free?: River -> Boolean
;;consumes a river system and returns true if no river in the system has an algal bloom
(define (algae-free? a-river)
  (if (boolean=? (river-bloom? a-river) #true)
      #true
      (algae-free-helper? (river-tributaries a-river))))

(check-expect (algae-free? amazon-river) #true);checks amazon river
(check-expect (algae-free? nile-river) #false);checks nile river
(check-expect (algae-free? empty-river) #false);checks empty river

;;algae-free-helper?: ListOfRiver -> Boolean
;;consumes a list of river and returns true if no river in the system has an algal bloom
(define (algae-free-helper? alor)
  (cond [(empty? alor) #false]
        [(cons? alor)(or  (algae-free? (first alor))
                          (algae-free-helper? (rest alor)))]))

(check-expect (algae-free-helper? (river-tributaries amazon-river)) #true);checks amazon tributaries
(check-expect (algae-free-helper? (river-tributaries nile-river)) #false);checks nile tributaries
(check-expect (algae-free-helper?  (river-tributaries empty-river)) #false);checks empty tributaries
         
;; 6. raise-all-ph
;;raise-all-ph: River -> River
;;consumes a river system and produces a river system that is the same as the original, except that the pH values of all the rivers in the system have been raised by 0.5 
(define (raise-all-ph a-river)
  (make-river
   (river-name a-river)
   (+ (river-ph a-river) 0.5)
   (river-bloom? a-river)
   (raise-all-ph-helper (river-tributaries a-river))))

(check-expect (raise-all-ph volga-river) (make-river "Volga" 10 #true (list)));checks Volga River

(check-expect (raise-all-ph nile-river) (make-river "Nile" 2.5 #false
                                                    (list (make-river "Yangtze" 5.5 #false
                                                                      (list))
                                                          (make-river "Congo" 7.5 #false
                                                                      (list)))));checks Nile River

(check-expect (raise-all-ph amazon-river) (make-river "Amazon" 7.5 #false
                                                      (list (make-river "Mississippi" 9.5 #true
                                                                        (list (make-river "Euphrates" 10.5 #false
                                                                                          (list))))
                                                            (make-river "Nile" 2.5 #false
                                                                        (list (make-river "Yangtze" 5.5 #false
                                                                                          (list))
                                                                              (make-river "Congo" 7.5 #false
                                                                                          (list))))
                                                            (make-river "Tigris" 8.5 #false
                                                                        (list (make-river "Volga" 10 #true
                                                                                          (list)))))));checks Amazon River

;;raise-all-ph-helper: ListOfRiver -> ListOfRiver
;;consumes a list of rivers and produces a list of rivers that is the same as the original, except that the pH values of all the rivers in the list have been raised by 0.5 
(define (raise-all-ph-helper alor)
  (cond [(empty? alor) empty]
        [(cons? alor) (append (list (raise-all-ph (first alor))) (raise-all-ph-helper (rest alor)))]))

(check-expect (raise-all-ph-helper (river-tributaries volga-river)) (list));checks Volga River

(check-expect (raise-all-ph-helper (river-tributaries nile-river))
              (list (make-river "Yangtze" 5.5 #false
                                (list))
                    (make-river "Congo" 7.5 #false
                                (list))));checks Nile River

(check-expect (raise-all-ph-helper (river-tributaries amazon-river))
              (list (make-river "Mississippi" 9.5 #true
                                (list (make-river "Euphrates" 10.5 #false
                                                  (list))))
                    (make-river "Nile" 2.5 #false
                                (list (make-river "Yangtze" 5.5 #false
                                                  (list))
                                      (make-river "Congo" 7.5 #false
                                                  (list))))
                    (make-river "Tigris" 8.5 #false
                                (list (make-river "Volga" 10 #true
                                                  (list))))));checks Amazon River


;; 7. find-subsystem
;;find-subsystem String River -> River OR false
;;consumes the name of a river and a river and produces either a river or false. The function returns the portion of the original river that has the named river as its root. If there is no river with the given name, the function returns false 
(define (find-subsystem name a-river)
  (if (string=? (river-name a-river) name)
      (make-river
       (river-name a-river)
       (river-ph a-river)
       (river-bloom? a-river)
       (river-tributaries a-river))
      (find-subsystem-helper name (river-tributaries a-river))))

(check-expect (find-subsystem "Nile" nile-river) (make-river "Nile" 2 #false
                                                             (list (make-river "Yangtze" 5 #false
                                                                               (list))
                                                                   (make-river "Congo" 7 #false
                                                                               (list)))));checks if "Nile" river exists in the list
(check-expect (find-subsystem "Congo" nile-river) (make-river "Congo" 7 #false (list)));checks if "Congo" river exists in the list
(check-expect (find-subsystem "Amazon" nile-river) #false);checks if "Amazon" river exists in list
(check-expect (find-subsystem "Volga" empty-river) #false);checks if "Volga" river exists in the list



;;find-subsystem-helper String ListOfRiver -> River OR false
;;consumes the name of a river and a list of rivers and produces either a river or false. The function returns the portion of the original river that has the named river as its root. If there is no river with the given name, the function returns false 
(define (find-subsystem-helper name alor) (cond [(empty? alor) #false]
                                                [(cons? alor)    (if (river? (find-subsystem name (first alor)))
                                                                     (find-subsystem name (first alor))           
                                                                     (find-subsystem-helper name (rest alor)))]))

(check-expect (find-subsystem-helper "Congo" (river-tributaries nile-river)) (make-river "Congo" 7 #false (list)));checks if "Congo" river exists in tributaries
(check-expect (find-subsystem-helper "Nile" (river-tributaries nile-river)) #false);checks if "Nile" river exists in tributaries
(check-expect (find-subsystem-helper "Volga" (river-tributaries empty-river)) #false);checks if "Volga" river exists in tributaries



;; Part 2: Higher Order Functions

(define-struct merchandise (name kind autographed? quantity price))
;; a Merchandise is a (make-merchandise String String Boolean Natural Number)
;; interp:
;;        Merchandise represents an item sold at a pop culture emporium, where
;;        name is the name of the merchandise item
;;        kind indicates whether the item is an action figure, board game, costume,
;;        manga/comic book, trading card, etc.
;;        autographed? is true if the item is autographed
;;        quantity is the numFber of that item that is being purchased
;;        price is the cost of a single item

;; a Receipt (ListOfMerchandise) is one of
;;  empty
;; (cons Merchandise Receipt)


;; 8. bargain-items
;; bargain-items: ListOfMerchandise -> ListOfString
;; consumes a list of merchandise items and produces a list of the names of all the items with a price under $10
(define (bargain-items alom)
  (local
    [(define (bargain-items? a-merchandise)(> 10 (merchandise-price a-merchandise)))]
    (map merchandise-name (filter bargain-items? alom))))
                            
(check-expect (bargain-items (list (make-merchandise "Clue" "board game" #false 250 15) (make-merchandise "Pokémon" "trading card" #true 100 5) (make-merchandise "Batman" "comic" #true 750 12))) (list "Pokémon"));checks clue, pokemon, and batman
(check-expect (bargain-items (list)) (list));checks empty list
(check-expect (bargain-items (list (make-merchandise "Superman" "comic" #false 900 10) (make-merchandise "Monoploy" "board game"#false 100 25) (make-merchandise "Batman" "comic" #true 750 12))) (list));checks superman, monopoly, and batman

;; 9. any-of-kind?
;; any-of-kind?: ListOfMerchandise String -> Boolean
;; consumes a ListOfMerchandise and a kind of merchandise item produces true if there is an item of that kind in the ListOfMerchandise
(define (any-of-kind? alom string)
  (local
    [(define (right-kind? a-merchandise) (string=? string (merchandise-kind a-merchandise)))]
    (cond [(empty? alom) #false]
          [(cons? alom) (ormap right-kind? alom)])))

(check-expect (any-of-kind? (list (make-merchandise "Clue" "board game" #false 250 15) (make-merchandise "Pokémon" "trading card" #true 100 5) (make-merchandise "Batman" "comic" #true 750 12)) "trading card") #true);checks clue, pokemon, and batman for a trading card
(check-expect (any-of-kind? (list (make-merchandise "Clue" "board game" #false 250 15) (make-merchandise "Pokémon" "trading card" #true 100 5) (make-merchandise "Batman" "comic" #true 750 12)) "costume") #false);checks clue, pokemon, and batman for a costume
(check-expect (any-of-kind? (list) "board game") #false);checks empty list for a board game

;; 10. list-cheap-autograph?
;; list-cheap-autograph: ListOfMerchandise Number -> ListOfMerchandise
;; consumes a list of merchandise items and returns a list of those autographed items that cost at most the given amount
(define (list-cheap-autograph? alom number)
  (local
    [(define (cheap-autograph? a-merchandise) ;; merch -> boolean
       (and (boolean=? (merchandise-autographed? a-merchandise) #true)
            (<= (merchandise-price a-merchandise) number)))]
    (cond [(empty? (filter cheap-autograph? alom)) (list)]
          [(cons? (filter cheap-autograph? alom))
           (append (list (first (filter cheap-autograph? alom)))
                   (list-cheap-autograph? (rest (filter cheap-autograph? alom)) number))])))

(check-expect (list-cheap-autograph? (list) 0) (list)) ; 0 threshold cost, empty list
(check-expect (list-cheap-autograph? (list) 100000) empty) ; 100000 threshold cost, empty list
(check-expect (list-cheap-autograph? (list (make-merchandise "Clue" "board game" #false 250 15)
                                           (make-merchandise "Pokémon" "trading card" #true 100 5) (make-merchandise "Batman" "comic" #true 750 12)) 0) empty) ; 0 threshold cost, populated list
(check-expect (list-cheap-autograph?
               (list (make-merchandise "Clue" "board game" #false 250 15)
                     (make-merchandise "Pokémon" "trading card" #true 100 5)
                     (make-merchandise "Batman" "comic" #true 750 12)) 1000)
              (list (make-merchandise "Pokémon" "trading card" #true 100 5)
                    (make-merchandise "Batman" "comic" #true 750 12))) ;1000 threshold cost, populated list
(check-expect (list-cheap-autograph?
               (list (make-merchandise "Clue" "board game" #false 250 15)
                     (make-merchandise "Pokémon" "trading card" #true 100 5)
                     (make-merchandise "Batman" "comic" #true 750 12)) 100)
              (list (make-merchandise "Pokémon" "trading card" #true 100 5)
                    (make-merchandise "Batman" "comic" #true 750 12))) ;100 threshold cost, populated list
