;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname szeto-z-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;zdszeto Zachary Szeto

; 1.
(define-struct message (username text read?))
;; a message is a (make-message String String Boolean)
;; interp: represents a message constrictor where
;;  username is the username of the sender
;;  text is the text of the message
;;  read? returns true if the user has read the message otherwise it returns false

(define-struct user (username mailbox))
;; a user is a (make-user String ListOfMessage)
;; interp: represents a user constrictor where
;;  username is the username of the sender
;;  mailbox is a list of messages that the user has received

; given:
;; a mailbox (ListOfMessage) is one of:
;; - empty, or
;; - (cons Message ListOfMessage)
;;
;; an email-system (ListOfUser) is one of:
;; - empty, or
;; - (cons User ListOfUser)


; 2.
;;mailsys is a list of users
;;remembers information about each user in the mail system
(define mailsys (list))

;;newuser is a new user in the mail system
;;stored as a data type User in mailsys
(define newuser (make-user "Newuser" (list)))

; 3.
;;add-new-user: String -> Void
;;consumes a username and produces void. The effect of the function is to add a new user with the given username to the mail system.
(define (add-new-user a-username)
  (set! mailsys (append (list(add-new-user-helper a-username)) mailsys)))

;;add-new-user-helper: String -> User
;;consumes a username and returns a user with that username and an empty mailbox
(define (add-new-user-helper a-username)
  (make-user a-username (list)))

(check-expect (add-new-user-helper "Jack Power Shanks") (make-user "Jack Power Shanks" (list)))
(check-expect (add-new-user-helper "Ian") (make-user "Ian" (list)))
(check-expect (add-new-user-helper "Zach") (make-user "Zach" (list)))
; 4.
;; send-email-message: String String String -> void
;; consumes the name of the sender of an email, the name of the recipient of the email, and the text of an email message, and produces void
;; EFFECT: stores a new unread message in the recipient's mailbox
(define (send-email-message a-sender a-recipient a-text)
  (local [(define (update-mail a-user) (if (string=? a-recipient (user-username a-user))
                                           (make-user a-recipient (cons (new-message a-sender a-text) (user-mailbox a-user)))
                                           a-user))]
    (set! mailsys (map update-mail mailsys))))
    
;; new-message: String String -> Message
;; consumes a sender's username and some text and produces message
(define (new-message a-sender a-text)
  (make-message a-sender a-text #false))

(check-expect (new-message "Zach" "Hi Ian") (make-message "Zach" "Hi Ian" #false))
(check-expect (new-message "Ian" "Hi Zach") (make-message "Ian" "Hi Zach" #false))
(check-expect (new-message "Jack Powers Shanks froms the 5th" "Hi Ian") (make-message "Jack Powers Shanks froms the 5th" "Hi Ian" #false))

; 5.
;;get-all-unread-messages: 
;;consumes a username and produces a list of messages
;;Effect: produces a list that contains the unread messages in the mailbox of the user with the given name and all unread messages in the named user's mailbox have been set to read
(define (get-all-unread-messages a-username)
  (local [(define (user-finder alou)
            (cond [(empty? alou) (error "User DNE")]
                  [(cons? alou) (if (string=? a-username (user-username (first alou)))
                                    (first alou)
                                    (user-finder (rest alou)))]))]
    (filter not-read? (user-mailbox (user-finder mailsys)))))

;;not-read?: Message -> Message
;;consumes a message and produces the same message but with the message-read? parameter set as true
(define (not-read? a-message)
  (local [(define opp (not (message-read? a-message)))]
    (begin (set-message-read?! a-message #true) opp)))

(check-expect (not-read? (make-message "Zach" "Hi Ian" #false))#true)
(check-expect (not-read? (make-message "Ian" "Hi Zach" #true))#false)
(check-expect (not-read? (make-message "" "" #false))#true)

; 6.
;;most-total-messages: (void) -> User
;;consume nothing and produces the user in the mailsystem with the largest number of messages in his/her mailbox
(define (most-total-messages)
  (local [(define num-messages 0) ;the number of messages a user has
          (define user-most (make-user "accumulator" empty)) ;the most messages out of all users
          (define (user-messages alou)
            (cond [(empty? alou) user-most]
                  [(cons? alou) (if (> (length (user-mailbox (first alou))) (length (user-mailbox user-most)))
                                    (begin (set! user-most (first alou)) (user-messages (rest alou)))
                                    (user-messages (rest alou)))]))]
    (if (empty? mailsys)
        (error "No Users in System")
        (user-messages mailsys))))

        
;7. (bonus)

; 7.3
;(check-expect (add-new-user "Todd") (void))
;(check-expect (add-new-user "78993") (void))
;(check-expect (add-new-user "") (void))


; 7.4
; (check-expect (send-email-message "Todd" "James" "oi bruv") (void))
; (check-expect (send-email-message "887773" "52749" "999009") (void))
; (check-expect (send-email-message "" "" "") (void))


; 7.5
; (check-expect (get-all-unread-messages "Attila") (list (make-message "Attila" "You must be quackers!" true)))


; 7.6
; (check-expect (most-total-messages) "Attila")
  

; 8.
;; sum-of-string-lengths: ListOfString -> Number
;; consumes a list of strings and produces the sum of the lengths of the strings
(define (sum-of-string-lengths alos)
  (sum-acc alos 0))

;; sum-acc: ListOfString Number -> Number
;; consumes a list of strings and an accumulator and produces the sum of the lengths of the strings
(define (sum-acc alos sum-in-counting)
  (cond [(empty? alos) sum-in-counting]
        [(cons? alos) (sum-acc (rest alos)(+ (string-length (first alos)) sum-in-counting))]))

(check-expect (sum-acc (list "ball" "real" "xd") 0) 10)
(check-expect (sum-acc (list "" "" "") 0) 0)

(check-expect (sum-of-string-lengths (list "trap" "snap" "flap")) 12)
(check-expect (sum-of-string-lengths (list "" "" "")) 0)


; 9.
;; one-long-string: ListOfString -> String
;; consumes a list of strings and produces one long string in the order they appear
(define (one-long-string alos)
  (string-acc alos ""))

;; string-acc: ListOfString String -> String
;; consumes a list of strings and an accumulator and produces a combined string
(define (string-acc alos long-string)
  (cond [(empty? alos) long-string]
        [(cons? alos) (string-acc (rest alos)(string-append long-string (first alos)))]))

(check-expect (string-acc (list "bro" "lab") "") "brolab")
(check-expect (string-acc (list "" "") "") "")

(check-expect (one-long-string (list "bro" "lab")) "brolab")
(check-expect (one-long-string (list "" "")) "")
