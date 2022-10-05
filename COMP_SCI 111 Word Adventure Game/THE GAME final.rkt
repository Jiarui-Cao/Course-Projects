;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Adventure Final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")

;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives)
  
  #:methods
  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))
  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void))))

;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (void))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (void))
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> void
  ;; EFFECT: prints the contents of the container
  (define (describe-contents container)
    (begin (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 (printf "There's nothing here.~%")
                 (begin (printf "You see:~%")
                        (for-each print-description other-stuff))))
           (void))))

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)
  ())

;; new-room: string -> room
;; Makes a new room with the specified adjectives
(define (new-room adjectives)
  (make-room (string->words adjectives)
             '()))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location)
  
  #:methods
  (define (examine thing)
    (print-description thing))

  ;; prepare-to-move!: thing container -> void
  ;; Called by move when preparing to move thing into
  ;; container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-move! container thing)
    (void)))

;; initialize-thing!: thing -> void
;; EFFECT: adds thing to its initial location
(define (initialize-thing! thing)
  (add! (thing-location thing)
        thing))

;; new-thing: string container -> thing
;; Makes a new thing with the specified adjectives, in the specified location,
;; and initializes it.
(define (new-thing adjectives location)
  (local [(define thing (make-thing (string->words adjectives)
                                    '() location))]
    (begin (initialize-thing! thing)
           thing)))








;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)
  ;; destination: container
  ;; The place this door leads to
  (destination)
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  (define (go door)
    (begin (move! me (door-destination door))
           (look))))

;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 room2 adjectives2)
  (local [(define r1->r2 (make-door (string->words adjectives1)
                                    '() room1 room2))
          (define r2->r1 (make-door (string->words adjectives2)
                                    '() room2 room1))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  ())

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives location)
  (local [(define person
            (make-person (string->words adjectives)
                         '()
                         location))]
    (begin (initialize-person! person)
           person)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;;;
;;; PROP
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   ;; examine-text: string
   ;; Text to print if the player examines this object
   examine-text
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop))

  (define (examine prop)
    (display-line (prop-examine-text prop))))

;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives '() location noun examine-text))]
    (begin (initialize-thing! prop)
           prop)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADD YOUR TYPES HERE!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ITEM: 
;;; the base type of all discrete units that could be used by the player. E.g. food, water, lock, hammer
;;; subtype of thing
(define-struct (item thing)
  (function)
  #:methods
  ; when you exmaine an item, you also get to view its functions
  (define (examine an-item)
    (begin (printf "You see ~A" (description an-item))
           (newline)
           (printf "It seems to be used to ~A." (item-function an-item))))
  ; heat: item1 item2 -> string, unless otherwise defined in subtypes
  (define (heat item1 item2)
    (printf "~A cannot be heated." (description item1))))

;; new-item: (listof strings) container string -> item
;; Creates a new item with specified descriptions
(define (new-item adjectives location function)
  (local ([define the-item (make-item adjectives
                                      '()
                                      location
                                      function)])
    (begin (initialize-thing! the-item)
           the-item)))




;;; CLOSET:
;;; Functions like a box. It can be opened and closed, but doesn't have any lock attached to it
;;; subtype of thing
(define-struct (closet thing)
  (open?)
  
  #:methods
  (define (open c)
    (set-closet-open?! c #true))
  (define (close c)
    (set-closet-open?! c #false))
  ;; A closer examination that lets you know what's inside the closet
  (define (look-inside c)
    (if (closet-open? c)
        (if (empty? (container-accessible-contents c))
            (printf "There's nothing here.~%")
            (begin (printf "You look inside and see:~%")
                   (for-each print-description (container-accessible-contents c))))
        (cond [(safe? c) (printf "Seems like the safe is closed. Open it up!")]
              [(drawer? c) (printf "Seems like the drawer is closed. Open it up!")]
              [(closet? c) (printf "Seems like the closet is closed. Open it up!")]))))


;; new-closet: (listof strings) container -> closet
;; Creates a new closet with specified descriptions
(define (new-closet adjectives location)
  (local ([define the-closet (make-closet adjectives
                                          '()
                                          location
                                          #false)])
    (begin (initialize-thing! the-closet)
           the-closet)))




;;; DRAWER:
;;; Functions like a safe. It can be opened and closed, and has a lock attached to it
;;; subtype of drawer
(define-struct (drawer closet)
  (locked? keyhole)
  
  #:methods
  ;; This is a intermediary procedure. NOT USER COMMAND!
  (define (unlock! d key-value)
    (if (= (drawer-keyhole d) key-value)
        (begin (set-drawer-locked?! d #false)
               (printf "You successfully unlocked the drawer!"))
        (begin (printf "Wrong key!"))))
  
  ;; Try to open the drawer. Only works when the drawer is unlocked
  (define (open d)
    (if (drawer-locked? d)
        (if (safe? d)
            (printf "The safe is locked. You can't open it now.")
            (printf "The drawer is locked."))
        (set-closet-open?! d #true))))


;; new-drawer: (listof strings) container boolean number -> drawer
;; Creates a new drawer with specified descriptions
(define (new-drawer adjectives location locked? keyhole)
  (local [(define the-drawer (make-drawer adjectives
                                          '()
                                          location
                                          #false
                                          locked?
                                          keyhole))]
    (begin (initialize-thing! the-drawer)
           the-drawer)))




;;; SAFE
;;; Similar to drawer. The only difference is that it has a 4-digit combination as opposed to "keyhole" for drawer
;;; Sidenote: drawer has this new field called "integrity". If you use (hammer) to hit (safe) for long enough,
;;; the integrity of safe will eventually decrease to 0 and you can open it freely.
(define-struct (safe drawer)
  (integrity)
  
  #:methods
  ;; Unlock the safe using a specified 4-digit combination
  (define (unlock s combination)
    (if (= (drawer-keyhole s) combination)
        (begin (set-drawer-locked?! s #false)
               (printf "success!"))
        (printf "Wrong combination!")))
  
  ;; Using hammer to smash the safe to eventually open it
  (define (smash s tool)
    ;; THIS PART NEEDS TO BE MODIFIED BASED ON ANDREW'S CODES ON TOOL!!!!
    (if (hammer? tool)
        (if (have? tool)
            ;; With each hit of hammer, the integrity of safe decrease by 1. Different messages will be displayed
            ;; depending on different integrity values. When integrity hits 0, the safe is opened by brute force
            (begin (set-safe-integrity! s (- (safe-integrity s) 1))
                   (cond [(>= (safe-integrity s) 20) (printf "The lock is too strong to be broken by brute force. I might just die trying.")]
                         [(and (< (safe-integrity s) 20) (>= (safe-integrity s) 12)) (printf "My arm sores... I'm waisting my time...")]
                         [(and (< (safe-integrity s) 12) (>= (safe-integrity s) 8)) (printf "The lock seems to be scratched...")]
                         [(and (< (safe-integrity s) 8) (>= (safe-integrity s) 4)) (printf "Wait... I feel the lock is a little bit loosened now")]
                         [(= (safe-integrity s) 3) (printf "This might actually be working... hitting it with a hammer")]
                         [(= (safe-integrity s) 2) (printf "Almost there...")]
                         [(= (safe-integrity s) 1) (printf "Please tell me it will work! I'm exhausted now...")]
                         [(= (safe-integrity s) 0) (begin (printf "OH MY GOD its broken!!!")
                                                          (newline) 
                                                          (printf "I can finally leave this stupid place!!!")
                                                          (set-drawer-locked?! s #false))]
                         [(< (safe-integrity s) 0) (printf "No need to hit it further...")]
                         ))
            (printf "You don't have the hammer. Take it into your inventory first."))
        (printf "doesn't work"))
    ))

;; new-safe: (listof strings) container number number -> safe
;; Creates a new safe with specified descriptions
(define (new-safe adjectives location combination integrity)
  (local [(define the-safe (make-safe adjectives
                                      '()
                                      location
                                      #false
                                      #true
                                      combination
                                      integrity))]
    (begin (initialize-thing! the-safe)
           the-safe)))




;;; KEY: It is an item with 2 unique fields: corresponding-lock and status
;;; Corresponding lock: a 8-digit implicit code used to check whether it matches with a drawer's lock
;;; Status: checks to see whether the key is inside its designated lock.
;;;    If it's inside the correct drawer lock, then status is 1
;;;    If it's not inside any lock, then status is 0
;;;    If it's inside the wrong lock (drawer or door), then status is -1
;;;    this field will return #true. If it's not inside any drawers' lock, this field will return #false
(define-struct (key item)
  (corresponding-lock status)
  
  #:methods
  ;; Put the key inside the keyhole of a drawer
  (define (insert! a-key a-drawer)
    (if (safe? a-drawer)
        (printf "This is a combination lock. There is no keyhole.")
        (cond [(= (key-status a-key) 0) (set-key-status! a-key 1)]
              [(= (key-status a-key) 1) (void)]
              [(= (key-status a-key) -1) (printf "This key is inside another lock, withdraw it first!")]
              )))

  ;; Turn the key to open the lock of drawer. There is a chance the key will break due to rusk, in which case
  ;; the game has to be restarted for the drawer to ever successfully be opened
  (define (turn! a-key)
    (if (< (random) 0.9)
        (begin (printf "The key has broken due to excessive force!")
               (destroy! a-key))
        (void)))

  ;; Take the key out of a drawer's lock
  (define (withdraw-key a-key)
    (begin (set-key-status! a-key 0)
           (printf "You removed the key from the lock"))))



;; Use a key on a locked drawer
(define (use-key a-key a-drawer)
  ;; Check to see if a-key is in (inventory)
  (if (have? a-key)
      ;; Check to see if the drawer is a safe. If it is, the procedure stops here
      (if (not (drawer? a-drawer))
          (printf "You can't use a key on things other than a drawer or a door.")
          ;; If the drawer is not a safe, then proceed to insert the key into the drawer
          (begin (insert! a-key a-drawer)
                 (unless (safe? a-drawer)

                   ;; Random number generator that determines whether to destroy the key
                   (if (< (random) 0.05)
                       (begin (printf "The key has broken due to excessive force!")
                              (destroy! a-key))
                       ;; If the key is not destroyed, then proceed to unlock the drawer
                       (when (= (key-status a-key) 1)
                         (begin (unlock! a-drawer (key-corresponding-lock a-key))
                                (unless (= (drawer-keyhole a-drawer) (key-corresponding-lock a-key))
                                  (set-key-status! a-key -1))))))))
      (printf "You can't use this key now. It's not in your inventory.")))


;; new-key: (listof strings) container number -> key
;; Makes a new key with the specified descriptions and initializes it
(define (new-key adjectives location corresponding-lock)
  (local [(define the-key (make-key adjectives
                                    '()
                                    location
                                    "unlock something"
                                    corresponding-lock
                                    0))]
    (begin (initialize-thing! the-key)
           the-key)))





;;; LOCKED-DOOR
;;; Behaves similarly to door. The only difference is that locked-door could be locked, and require a key to open it (using the same logic as drawer).
(define-struct (locked-door door)
  (locked? corresponding-key)

  #:methods
  ;; Unlock door if the key's value matches (locked-door-corresponding-key)
  (define (unlock-door! LD key-value)
    (if (= (locked-door-corresponding-key LD) key-value)
        (begin (set-locked-door-locked?! LD #false)
               (printf "You unlocked the door!"))
        (printf "Wrong key!")))
  
  ;; Open the door and go through. However, you can't go through if the door is locked
  (define (go LD)
    (if (locked-door-locked? LD)
        (printf "The door is locked. You need to find its key.")
            (begin (move! me (door-destination LD))
                   (printf "You freed yourself from the locked, dreadful bedroom!")
                   (newline)
                   (printf "Congratulations!")))))
    
  

;;; join-locked!: location (listof strings) location number -> 2 doors
;;; Make 2 doors connecting room1 and room2
;;; On one side, the door is locked, requiring a key to open; on the other side, the door is unlocked
(define (join-locked! room1 adjectives1 room2 corresponding-key)
  (local [(define r1->r2 (make-locked-door adjectives1 '() room1 room2 #true corresponding-key))
          (define r2->r1 (make-locked-door adjectives1 '() room2 room1 #false corresponding-key))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

;;; This is the user command (unlock-door). It takes a door and a key as input.
(define (unlock-door a-key LD)
  (if (have? a-key)
      (if (key? a-key)
          (begin (unlock-door! LD (key-corresponding-lock a-key))
                 (set-key-status! a-key -1))
          (printf "You need a key to open the door"))
      (printf "You can't use this key now. It's not in your inventory.")))
      
      



;;; BedTable
;;; 
(define-struct (bedTable thing)
  (moved?)

  #:methods
  (define (push bT)
    (begin
      (set-bedTable-moved?! bT #t)
      (printf "Now you can move the bed. Take a try!"))))

(define (new-bedTable adjective location)
  (local ([define the-bedTable (make-bedTable adjective
                                              '()
                                              location
                                              #false)])
    (begin (initialize-thing! the-bedTable)
           )))
    




;;; Bed
;;; 
(define-struct (bed thing)
  (drag?)
  #:methods
  (define (drag b)
    (if (eq? #t (bedTable-moved? (the bedTable)))

        (begin
          (set-bed-drag?! b #t)
          (new-notebook "old" (here) "1")
          (printf "There seems to be something under the bed. Take a look!"))

        (printf "Oops! The bed is jammed. Move the bedside table"))))


(define (new-bed adjective location)
  (local ([define the-bed (make-bed adjective
                                    '()
                                    location
                                    #false)])
    (begin (initialize-thing! the-bed)
           the-bed)))





;;; Notebook
;;;
(define-struct (notebook thing)
  (codes)
  #:methods
  (define (examine n)
    (printf "You looked at the notebook. You saw the number ~A. ~nThis is one of the code number!" (notebook-codes n))
    )
  )

(define (new-notebook adjective location number)
  (local ([define nb (make-notebook (string->words adjective)
                                    '()
                                    location
                                    number)])
    (begin (initialize-thing! nb)
           )))






;;; LAMP:
;;; an item that can
;;; 1) light up the ceiling
;;; 2) heat up the note

(define-struct (lamp item)
  (on? temperature)
  #:methods
  ;; switch: lamp -> void
  ;; flip the on/off status and temperature of the lamp

  (define (switch lamp)
    (if (lamp-on? lamp)
        (begin
          (set-lamp-on?! lamp #f)
          (set-lamp-temperature! lamp 25))
        (begin
          (set-lamp-on?! lamp #t)
          (set-lamp-temperature! lamp 300))))
  ;; examine: lamp -> void
  ;; gives the description of the lamp and tell the player whether the lamp is on or off
  (define (examine lamp)
    (begin
      (printf "You see ~A~%" (description lamp))
      (printf "It seems to be used to ~A.~%" (item-function lamp))
      (if (lamp-on? lamp)
          (printf "It is on.~%")
          (printf "It is off.~%"))
      (printf "It has a temperature of ~A C." (lamp-temperature lamp))))
  ;; touch/move whatever: lamp -> void/error (end of the game)
  ;; the play will die immediately if they touch a heated lamp. if the lamp is unheated, nothing will happen
  (define (touch lamp)
    (when (lamp-on? lamp)
      (begin (printf "You died as you touched a hot lamp")
             (exit))))
  )

(define (new-lamp description location function on? temperature)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define lamp (make-lamp adjectives '() location function on? temperature))]
    (begin (initialize-thing! lamp)
           lamp)))





;;; NOTE
;;; a piece of paper with writings on it. The visible writings can be seen when the note
;;is examined. The invisible writing can only be seen after heated by a switched-on lamp
(define-struct (note item)
  (heated? visible-writings invisible-writings)
  #:methods
  ;;heat: note, any->void
  ;;If note has been heated before, tell the player that there is no need to heat it again
  ;;If not...
  ;;change heated? to #t if any is a switched-on lamp. 
  ;;Otherwise tell the player there is no heating source
  (define (heat note any)
    (if (note-heated? note)
        (printf "The note has already been heated. There is no need to heat it up again.~%")
        (if (eqv? (the lamp) any)
            (if (= (lamp-temperature (the lamp)) 300)
                (begin 
                  (set-note-heated?! note #t)
                  (printf "The note is heated. Call (examine (the note)) again for new information."))
                (printf "There is no heat source.~%"))
            (printf "There is no heat source.~%"))))             
                            
  ;;examine: print out the description of the note and visible writings on it. If the note is heated by the lamp,
  ;; the invisible message will also appear and be visible permenantly afterwards.  
  (define (examine note)
    (if (note-heated? note)
        (begin
          (printf "You see ~A~%" (description note))
          (printf "It says...~%")
          (printf (note-visible-writings note))
          (newline)
          (printf (note-invisible-writings note))
          (void))
        (begin
          (printf "You see ~A~%" (description note))
          (printf "It says...~%")
          (printf (note-visible-writings note))
          (void)))))
  
(define (new-note description location function heated? visible-writings invisible-writings)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define note (make-note adjectives '() location function heated? visible-writings invisible-writings))]
    (begin (initialize-thing! note)
           note)))






;;; CEILING: it has important information to find out one number you need to unlock the bulky black safe
(define-struct (ceiling thing)
  (answer)
  #:methods
  ;; examine: see nothing if lamp-on? is #f
  ;; see ceiling-answer if lamp-on? is #t
  (define (examine ceiling)
    (if (lamp-on? (the lamp))
        (begin
          (printf "You see ~A" (description ceiling))
          (newline)
          (printf "But since you have switched on the lamp, you can see some faint writings on the ceiling:~%")
          (printf (ceiling-answer ceiling))
          )          
        (begin
          (printf "You see ~A" (description ceiling))
          (newline)))))
  
(define (new-ceiling description location answer)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define ceiling (make-ceiling adjectives '() location answer))]
    (begin (initialize-thing! ceiling)
           ceiling)))






;;; VASE: a thing that contains rose and water.
(define-struct (vase thing)
  ()
  
  #:methods
  ;; Gives a description of the vase and the things contian in the vase (rose and water);
  ;; Also provides some hints for what the player should do next
  (define (examine vase)
    (begin (printf "You see ~A" (description vase))
           (newline)
           (printf "It seems to be used to hold water and withered dark red colored roses")
           (newline)
           (printf "The 2 withered roses, even though in water, seems to have the same amount of petals on each, hmmmmmm maybe try counting the petals in the vase")
           (newline)
           (printf "The vase is also seems to be lightweighted, perhaps I should move it")))

  ;; count the number of petals on each rose
  (define (count vase)
    (begin (printf "There are 3 petals on each rose. I wonder if the total number of petals mean anything")
           (newline)
           (printf "This water smells like Fiji natural water, maybe I should not drink from the vase")))

  ;; drink -> void/error (end of the game)
  (define (drink vase)
    (begin (printf "You died as you drank poisoned water that has Fiji natural water's scent")
           (exit)))

  ;; Attempt to move the vase and discover what's underneath the vase
  (define (move vase)
    (begin (printf "There is nothing under the vase"))))


(define (new-vase description location)
  (local ([define the-vase (make-vase description
                                      '()
                                      location
                                      )])
    (begin (initialize-thing! the-vase)
           the-vase)))







;;; HAMMER: a tool that can be used smash things/items
(define-struct (hammer item)
  (broken? healthuntilbroken)
  #:methods
  ;; Gives a description of the hammer and its usage
  (define (examine hammer)
    (begin (printf "You see ~A" (description hammer))
           (newline)
           (printf "You may use this hammer to smash things/items (hint: try the safe).")))
    )

(define (new-hammer adjectives location broken? hammerfunction healthuntilbroken)
  (local ([define the-hammer (make-hammer adjectives
                                          '()
                                          location
                                          broken?
                                          hammerfunction
                                          healthuntilbroken)])
    (begin (initialize-thing! the-hammer)
           the-hammer)))








 








;;;
;;; USER COMMANDS
;;;

(define (look)
  (begin (printf "You are in ~A.~%"
                 (description (here)))
         (describe-contents (here))
         (void)))

(define-user-command (look) "Prints what you can see in the room")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define (take thing) 
  (if (closet? (thing-location thing))
      (if (closet-open? (thing-location thing))
          (move! thing me)
          (printf "You don't have access to this item"))
      (move! thing me)))

(define-user-command (take thing)
  "Moves thing to your inventory")

(define (drop thing)
  (move! thing (here)))

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container.")

(define (help)
  (for-each (λ (command-info)
              (begin (display (first command-info))
                     (newline)
                     (display (second command-info))
                     (newline)
                     (newline)))
            (all-user-commands)))

(define-user-command (help)
  "Displays this help information")

(define-user-command (go door)
  "Go through the door to its destination")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check condition)
  "Throws an exception if condition is false.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADD YOUR COMMANDS HERE!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-user-command (look-inside closet/drawer/safe)
  "Have a peak inside the container to find what's inside")

(define-user-command (open closet/drawer/safe)
  "Opens the closet (or drawer) so you can see what's inside of it")

(define-user-command (close closet/drawer/safe)
  "Closes the closet (or drawer)")

(define-user-command (unlock safe combination)
  "Enter a 4-digit code to unlock the safe")

(define-user-command (smash safe hammer)
  "Try hitting the safe with hammer. Although it might not work...")

(define-user-command (use-key key drawer)
  "Try unlocking the drawer using the key")

(define-user-command (withdraw-key key)
  "Withdraw the key from the lock it's inside now")

(define-user-command (unlock-door key door)
  "Use the key to unlock the door")

(define-user-command (heat note lamp)
  "Heat up the note only if the lamp is switched on.")

(define-user-command (switch lamp)
  "Switch on a lamp that is off, or swicth off a lamp that is on")

(define-user-command (touch lamp)
  "You will die immediately if the lamp is on. Otherwise nothing will happen.")

(define-user-command (push bedTable)
  "push the bed table in order to move the beds")

(define-user-command (drag bed)
  "drag the bed in order to find the notebook, which contains the code for room escape")

(define-user-command (examine vase)
  "Examine the vase")

(define-user-command (count vase)
  "Count the number of petals on each rose")

(define-user-command (drink vase)
  "Drink from the vase")

(define-user-command (move vase)
  "Move the vase to the side")

(define-user-command (examine hammer)
  "Examine the hammer")






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE GAME WORLD!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define starting-room (new-room ""))
          (define living-room (new-room "living"))
          (define the-closet (new-closet (list "gothic") starting-room))
          (define drawer1 (new-drawer (list "steel") the-closet #true 03416270))
          (define drawer2 (new-drawer (list "wooden") the-closet #false 00001111))
          (define (dolls adjectives location)
            (make-prop adjectives '() location "doll" "this is a ~A doll, I wonder why its here?"))
          (define rubiks-cube (new-prop "rubik's cube" "This is a 3x3 unshuffled rubiks cube. It's white side is facing upwards. I wonder why it is here" drawer2))
          (define the-safe (new-safe '("bulky" "black") starting-room 3716 25))
          (define bed (new-bed '("shaky") starting-room))
          (define bedTable (new-bedTable '("new") starting-room))
          (define the-vase (new-vase (list "Dark green colored hummingbird") starting-room))
          (define the-hammer (new-hammer (list "Metal-headed electric") starting-room #false "smash things/items" 30))]
    (begin (set! me (new-person "" starting-room))
           ;; Add join commands to connect your rooms with doors 
           ;; Add code here to add things to your rooms
           
           ;; Testing item
           ; (new-item (list "black" "peculiar") starting-room "open doors")
           
           ;; Creating (the gothic closet) and two drawers that are located inside (the gothic closet)
           the-closet
           drawer1
           drawer2
           
           ;; Creating multiple dolls of different colors and features, all located inside (the gothic closet)
           (for-each (λ (adj)
                       (begin (initialize-thing! (dolls (string->words adj) the-closet))
                              (dolls (string->words adj) the-closet)))
                     (list "green" "blue" "torn-up" "soft"))
           (begin (initialize-thing! (dolls '("creepy") drawer1))
                  (dolls '("creepy") drawer1))
           (begin (initialize-thing! (dolls '("red") drawer1))
                  (dolls '("red") drawer1))
           (begin (initialize-thing! (dolls '("gigantic") drawer1))
                  (dolls '("gigantic") drawer1))

           ;; Creating a rubik's cube
           rubiks-cube

           ;; Creating the locked drawer that contains the key to escape

           ;; Creating keys
           (new-key '("silver") the-closet 03416270) ; the key used to unlock the steel drawer
           (new-key '("obsidian") the-safe 51285639) ; the key used to unlock the final door
           ; Note: this key is located within (the bulky black safe)

           ;; Creating the door that joins the starting-room with living-room
           (join-locked! starting-room '("old" "iron") living-room 51285639)

           ;; DANLING
           (new-lamp "old with-a-switch heating lamp" starting-room "help you 1)see the ceiling; 2) heat up stuff" #f 25)
           (new-note "scrappy note" starting-room "give the hint of the game" #f
                     "I am glad that you find me! I am not a common piece of paper. Rather, I would help you escape.
The essential information was written by a magical ink that is visible only after heating..."
                     "Answer the question on the ceiling, count the total number of dolls in the closet, look under the bed, take an examination of the vase,
and use the information to unlock the safe. Good luck!")
           (new-ceiling "dark ceiling" starting-room "What is the closest integer to pi?~%")

           ;; ANDREW
           the-vase
           the-hammer

           ;; RICHARD
           bed
           bedTable
           
           
           (check-containers!)
           (void))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORLD FEATURE DESIGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;

(define-walkthrough win
  ;; Get the initial note that contains instructions on what you should do
  (switch (the lamp))
  (heat (the note) (the lamp))
  (examine (the note))
  (newline)

  ;; Examine the ceiling to get the FIRST digit of code (to open the final safe)
  (examine (the ceiling))
  (newline)

  ;; Count the total number of dolls in the closet to get the SECOND digit of code (to open the final safe)
  (open (the gothic closet))
  (look-inside (the gothic closet))
  (take (within (the gothic closet) key))
  (use-key (the silver key) (within (the gothic closet) drawer steel))
  (open (within (the gothic closet) drawer steel))
  (open (within (the gothic closet) drawer wooden))
  (look-inside (within (the gothic closet) drawer steel))
  (look-inside (within (the gothic closet) drawer wooden))
  (newline)
  
  ;; Examine the old notebook by draging the bed to get the THIRD digit of code (to open the final safe)
  (push (the bedTable))
  (drag (the bed))
  (look)
  (examine (the old notebook))
  (newline)

  ;; Count the total number of petals in the vase
  (examine (the vase))
  (newline)
  (count (the vase))
  (newline)

  ;; Open the safe, take the obsidian key inside it, and win the game!
  (unlock (the bulky black safe) 3716)
  (open (the bulky black safe))
  (look-inside (the bulky black safe))
  (take (within (the bulky black safe) key obsidian))
  (inventory)
  (newline)

  (unlock-door (the obsidian key) (the old iron locked-door))
  (go (the old iron locked-door))
  )



(define-walkthrough win2
  (take (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (smash (the safe) (the hammer))
  (newline)

  (open (the bulky black safe))
  (look-inside (the bulky black safe))
  (take (within (the bulky black safe) key obsidian))
  (inventory)
  (newline)

  (unlock-door (the obsidian key) (the old iron locked-door))
  (go (the old iron locked-door))
  )









;;;
;;; UTILITIES
;;;

;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (probe obj))))

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))

;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? first-char "aeiou"))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)
   
;;;
;;; Start it up
;;;

(start-game)
(look)


