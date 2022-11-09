;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname szeto-z-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Zachary Szeto zdszeto
;Ian Kane ibkane

;Problem 1a
(define-struct projectnode (project-id title advisor students left right))
;; a Projectnode is a (make-projectnode Number String String ListOfStudent BST BST )
;; interp: represents a projectnode constrictor where
;;  project-id is the unique project ID (the key value),
;;  title is the title of the project
;;  advisor is the name of the advisor
;;  students is a list of students working on the project
;;  left is the left subtree (a BST)
;;  right is the right subtree (a BST)

;; a BST is one of
;;   false
;;   ProjectNode
;; a ProjectNode is a (make-projectnode Number String String ListOfStudent BST BST)

;;A BST returns false if there are no nodes to the left or right of it (aka it is a leaf) otherwise a BST is a Projectnode containig a (make-projectnode Number String String ListOfStudent BST BST)

;;INVARIANT:
;;   key > all keys in left subtree
;;   key < all keys in right subtree
;;All keys are unique, i.e. no key appears twice in a BST

(define-struct student (name email))
;; a Student is a (make-student String String)
;; interp: represents a student constrictor where
;;  name is the name of the student
;;  email is the email adress of the student

;Problem 1b
;; a ListOfStudent is one of
;;    empty
;;    (cons Student ListOfStudent)

;Problem 1c
;Example Students
(define JOHN (make-student "John John" "JohnJohn@gmail.com"))
(define BOB (make-student "Bob Bob" "BobBob@gmail.com"))
(define JACK (make-student "Jack Jack" "JackJack@gmail.com"))
(define DOM (make-student "Dom Dom" "DomDom@gmail.com"))

;Example ListOfStudents
(define ListA (list JOHN BOB))
(define ListB (list JACK DOM))
(define ListEmpty (list))

;Problem 2
(define PROJECT-TREE (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" ListA ;gen 0
                                       (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" ListB ;left node gen 1
                                                         (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA ;left node gen 2
                                                                           #false ;left leaf gen 3
                                                                           #false) ;right left gen 3
                                                         (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB ;right node gen 2
                                                                           #false ;left leaf gen 3
                                                                           #false)) ;right left gen 3                
                                       (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA ;right node gen 1
                                                         (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                           #false ;left leaf gen 3
                                                                           #false) ;right left gen 3
                                                         (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" ListA
                                                                           #false ;left leaf gen 3
                                                                           #false)))) ;right left gen 3
(define EMPTY-TREE (make-projectnode 0 "" "" (list) #false #false))

;Problem 3
;;;template for functions on Student
;#;(define (fcn-for-Student   a-student)
;  (... (student-name         a-student)
;       (student-email        a-student)))

; ;; los-fcn:  ListOfStudent ->
; ;;
; (define (students-fcn: alos)
;   (cond [(empty? alos)  (...) ]
;         [(cons? alos)   (...        (los-fcn (first alos))
;                                     (los-fcn (rest alos)))]))

;;;(naive) template for functions on BinaryTrees:
;#;(define (fcn-for-BinaryTree a-tree)
;    (cond [(boolean? a-tree) (... )];base case
;          [(projectnode?  a-tree) (...
;                                  (projectnode-project-id a-tree)
;                                  (projectnode-title a-tree)
;                                  (projectnode-advisor a-tree)
;                                  (projectnode-students a-tree)
;                                  (projectnode-left a-tree)
;                                  (projectnode-right a-tree))])

;Problem 4
;;Signature: any-in-depth?: BST Number+ -> Boolean
;;Purpose: consumes a binary search tree and the number of a department, and produces true if any of the projects in the project database are from that department
(define (any-in-dept? a-bst dept)
  (cond [(boolean? a-bst) #false]
        [(projectnode? a-bst) (if (= dept (floor(projectnode-project-id a-bst)))
                                  #true
                                  (if (< dept (floor(projectnode-project-id a-bst)))
                                      (any-in-dept? (projectnode-left a-bst) dept)
                                      (any-in-dept? (projectnode-right a-bst) dept)))]))

;Test Cases
(check-expect (any-in-dept? PROJECT-TREE 17) #true);checks for dept number 17
(check-expect (any-in-dept? PROJECT-TREE 11) #true);checks for dept number 11
(check-expect (any-in-dept? PROJECT-TREE 19) #true);checks for dept number 19
(check-expect (any-in-dept? PROJECT-TREE 12) #true);checks for dept number 12
(check-expect (any-in-dept? PROJECT-TREE 9) #true);checks for dept number 9
(check-expect (any-in-dept? PROJECT-TREE 18) #true);checks for dept number 18
(check-expect (any-in-dept? PROJECT-TREE 52) #true);checks for dept number 52
(check-expect (any-in-dept? PROJECT-TREE 15) #false);checks for dept number 15
(check-expect (any-in-dept? PROJECT-TREE 20) #false);checks for dept number 20
(check-expect (any-in-dept? PROJECT-TREE 0) #false);checks for dept number 0
(check-expect (any-in-dept? PROJECT-TREE 100) #false);checks for dept number 100

;Problem 5
;;Signature: drop-student: BST Number+ String -> BST
;;Purpose: consumes a binary search tree, a project number, and the email address of a student, and produces a binary search tree
#(define (drop-student a-bst proj email)
   (cond [(boolean? a-bst) #false]
         [(projectnode? a-bst) (if (= proj (projectnode-project-id a-bst))
                                   (make-projectnode (projectnode-project-id a-bst)
                                                     (projectnode-title a-bst)
                                                     (projectnode-advisor a-bst)
                                                     (drop-student-in-list a-bst (projectnode-students a-bst) email)
                                                     (projectnode-left a-bst)
                                                     (projectnode-right a-bst))
                                   (if (< proj (projectnode-project-id a-bst))
                                       (make-projectnode (projectnode-project-id a-bst)
                                                         (projectnode-title a-bst)
                                                         (projectnode-advisor a-bst)
                                                         (projectnode-students a-bst)
                                                         (drop-student (projectnode-left a-bst) proj email)
                                                         (projectnode-right a-bst))
                                       (make-projectnode (projectnode-project-id a-bst)
                                                         (projectnode-title a-bst)
                                                         (projectnode-advisor a-bst)
                                                         (projectnode-students a-bst)
                                                         (projectnode-left a-bst)
                                                         (drop-student (projectnode-right a-bst) proj email))))]))
(define (drop-student a-bst proj email)
  (cond
    [(boolean? a-bst) #false]
    [(= proj (projectnode-project-id a-bst))
            (make-projectnode (projectnode-project-id a-bst)
                              (projectnode-title a-bst)
                              (projectnode-advisor a-bst)
                              (drop-student-in-list a-bst (projectnode-students a-bst) email)
                              (projectnode-left a-bst)
                              (projectnode-right a-bst))]
        [(projectnode? a-bst)                                  
         (if (< proj (projectnode-project-id a-bst))
             (make-projectnode (projectnode-project-id a-bst)
                               (projectnode-title a-bst)
                               (projectnode-advisor a-bst)
                               (projectnode-students a-bst)
                               (drop-student (projectnode-left a-bst) proj email)
                               (projectnode-right a-bst))
             (make-projectnode (projectnode-project-id a-bst)
                               (projectnode-title a-bst)
                               (projectnode-advisor a-bst)
                               (projectnode-students a-bst)
                               (projectnode-left a-bst)
                               (drop-student (projectnode-right a-bst) proj email)))]))           
;Test Cases
(check-expect (drop-student PROJECT-TREE 17.2205 "JohnJohn@gmail.com") (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" (list (make-student "Bob Bob" "BobBob@gmail.com"))
                                                                                         (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" ListB 
                                                                                                           (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA 
                                                                                                                             #false 
                                                                                                                             #false) 
                                                                                                           (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB 
                                                                                                                             #false
                                                                                                                             #false))               
                                                                                         (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA 
                                                                                                           (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                                                                             #false 
                                                                                                                             #false)
                                                                                                           (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" ListA
                                                                                                                             #false
                                                                                                                             #false)))) ; Removes person from project 17.2205

(check-expect (drop-student PROJECT-TREE 17.2205 "NateNate@gmail.com") (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" ListA
                                                                                         (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" ListB 
                                                                                                           (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA 
                                                                                                                             #false 
                                                                                                                             #false) 
                                                                                                           (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB 
                                                                                                                             #false
                                                                                                                             #false))               
                                                                                         (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA 
                                                                                                           (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                                                                             #false 
                                                                                                                             #false)
                                                                                                           (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" ListA
                                                                                                                             #false
                                                                                                                             #false)))) ; Doesn't remove anyone from project 17.2205

(check-expect (drop-student PROJECT-TREE 52.4891 "JohnJohn@gmail.com") (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" ListA
                                                                                         (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" ListB 
                                                                                                           (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA 
                                                                                                                             #false 
                                                                                                                             #false) 
                                                                                                           (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB 
                                                                                                                             #false
                                                                                                                             #false))               
                                                                                         (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA 
                                                                                                           (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                                                                             #false 
                                                                                                                             #false)
                                                                                                           (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" (list BOB)
                                                                                                                             #false
                                                                                                                             #false)))) ;Remove person from upper tree project

(check-expect (drop-student PROJECT-TREE 11.1302 "DomDom@gmail.com") (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" ListA
                                                                                       (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" (list (make-student "Jack Jack" "JackJack@gmail.com")) 
                                                                                                         (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA 
                                                                                                                           #false 
                                                                                                                           #false) 
                                                                                                         (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB 
                                                                                                                           #false
                                                                                                                           #false))               
                                                                                       (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA 
                                                                                                         (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                                                                           #false 
                                                                                                                           #false)
                                                                                                         (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" ListA
                                                                                                                           #false
                                                                                                                           #false)))) ; Removes person from lower tree project
(check-expect (drop-student PROJECT-TREE 1000 "") (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" ListA ;gen 0
                                                                    (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" ListB ;left node gen 1
                                                                                      (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA ;left node gen 2
                                                                                                        #false ;left leaf gen 3
                                                                                                        #false) ;right left gen 3
                                                                                      (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB ;right node gen 2
                                                                                                        #false ;left leaf gen 3
                                                                                                        #false)) ;right left gen 3                
                                                                    (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA ;right node gen 1
                                                                                      (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                                                        #false ;left leaf gen 3
                                                                                                        #false) ;right left gen 3
                                                                                      (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" ListA
                                                                                                        #false ;left leaf gen 3
                                                                                                        #false)))) ;right left gen 3


;;Signature: drop-student-in-list: BST ListOfStudent String -> ListOfStudent
;;Purpose: consumes a binary search tree, a list of students, and the email address of a student, and produces a list of students with a specified student removed
(define (drop-student-in-list a-bst alos email)
  (cond [(empty? alos) empty]
        [(cons? alos)
         (if (string=? email (student-email (first alos)))
             (rest alos)
             (cons (first alos) (drop-student-in-list a-bst (rest alos) email)))]))

;Test Cases
(check-expect (drop-student-in-list PROJECT-TREE ListA "BobBob@gmail.com") (list (make-student "John John" "JohnJohn@gmail.com"))) ; Removes 2nd person from ListA
(check-expect (drop-student-in-list PROJECT-TREE ListB "JackJack@gmail.com") (list (make-student "Dom Dom" "DomDom@gmail.com"))) ; Removes 1st person from ListB
(check-expect (drop-student-in-list PROJECT-TREE ListB "DomDom@gmail.com") (list (make-student "Jack Jack" "JackJack@gmail.com"))) ; Removes 2nd person from ListB
(check-expect (drop-student-in-list PROJECT-TREE ListA "NateNate@gmail.com") (list (make-student "John John" "JohnJohn@gmail.com") (make-student "Bob Bob" "BobBob@gmail.com"))) ; Does not remove bc not in list or definition
(check-expect (drop-student-in-list PROJECT-TREE ListA "DomDom@gmail.com") (list (make-student "John John" "JohnJohn@gmail.com") (make-student "Bob Bob" "BobBob@gmail.com"))) ; Does not remove bc not in list
(check-expect (drop-student-in-list PROJECT-TREE ListEmpty "DomDom@gmail.com") (list)) ; Does not remove bc empty list

;; Problem 6
;;Signature:list-projects-in-order-by-id-num: BST -> ListOfString
;;Purpose: consumes a binary search tree and produces a list of the titles of the projects
(define (list-projects-in-order-by-id-num a-bst)
  (cond
    [(boolean? a-bst) empty]
    [else
     (append
      (list-projects-in-order-by-id-num (projectnode-left a-bst))
      (list (projectnode-title a-bst))
      (list-projects-in-order-by-id-num (projectnode-right a-bst)))]))

;Test Cases
(check-expect (list-projects-in-order-by-id-num PROJECT-TREE) (list "Electric Bike" "Sail Boat" "Talking Robot" "Solar Oven" "Smart Camera" "Trash Collector" "Automatic Curtains"))
(check-expect (list-projects-in-order-by-id-num EMPTY-TREE) (list ""))
  

;; Problem 7
;;Signature: add-project: BST Number+ String String -> BST
;;consumes a binary search tree, a new project id, a new title, and a new advisor and produces a binary search tree the same as the original except that a new project with the given
;;project-id is inserted at the correct node where the new key value fits appropiately
(define (add-project a-bst proj title advisor)
  (cond [(boolean? a-bst) (make-projectnode proj title advisor (list) #false #false)]
        [(projectnode? a-bst) (if (< proj (projectnode-project-id a-bst))
                                  (make-projectnode
                                   (projectnode-project-id a-bst)
                                   (projectnode-title a-bst)
                                   (projectnode-advisor a-bst)
                                   (projectnode-students a-bst)
                                   (add-project (projectnode-left a-bst) proj title advisor)
                                   (projectnode-right a-bst))
                                  (make-projectnode
                                   (projectnode-project-id a-bst)
                                   (projectnode-title a-bst)
                                   (projectnode-advisor a-bst)
                                   (projectnode-students a-bst)
                                   (projectnode-left a-bst)
                                   (add-project (projectnode-right a-bst) proj title advisor)))]))

(check-expect (add-project PROJECT-TREE 1029.1029 "Solar Sails" "Advisor Heath")
              (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" ListA ;gen 0
                                (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" ListB ;left node gen 1
                                                  (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA ;left node gen 2
                                                                    #false ;left leaf gen 3
                                                                    #false) ;right left gen 3
                                                  (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB ;right node gen 2
                                                                    #false ;left leaf gen 3
                                                                    #false)) ;right left gen 3                
                                (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA ;right node gen 1
                                                  (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                    #false ;left leaf gen 3
                                                                    #false) ;right left gen 3
                                                  (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" ListA
                                                                    #false ;left leaf gen 3
                                                                    (make-projectnode 1029.1029 "Solar Sails" "Advisor Heath" (list) #false #false)))));inserted node

(check-expect (add-project PROJECT-TREE 50.1982 "Baking Soda Volcano" "Advisor Ian")
              (make-projectnode 17.2205 "Solar Oven" "Advisor Mary" ListA ;gen 0
                                (make-projectnode 11.1302 "Sail Boat" "Advisor Luke" ListB ;left node gen 1
                                                  (make-projectnode 9.9007 "Electric Bike" "Advisor Danny" ListA ;left node gen 2
                                                                    #false ;left leaf gen 3
                                                                    #false) ;right left gen 3
                                                  (make-projectnode 12.7201 "Talking Robot" "Advisor Jenifer" ListB ;right node gen 2
                                                                    #false ;left leaf gen 3
                                                                    #false)) ;right left gen 3                
                                (make-projectnode 19.1023 "Trash Collector" "Advisor Dennis" ListA ;right node gen 1
                                                  (make-projectnode 18.6940 "Smart Camera" "Advisor Logan" ListEmpty
                                                                    #false ;left leaf gen 3
                                                                    #false) ;right left gen 3
                                                  (make-projectnode 52.4891 "Automatic Curtains" "Advisor Sarah" ListA
                                                                    (make-projectnode 50.1982 "Baking Soda Volcano" "Advisor Ian" (list) #false #false);inserted node
                                                                    #false)))) ;right leaf gen 3
(check-expect (add-project EMPTY-TREE 1 "" "") (make-projectnode
                                                0
                                                ""
                                                ""
                                                (list)
                                                #false
                                                (make-projectnode
                                                 1
                                                 ""
                                                 ""
                                                 (list)
                                                 #false
                                                 #false)))
                                                                    
       





