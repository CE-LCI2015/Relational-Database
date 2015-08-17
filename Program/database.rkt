#lang scheme
(require "utilities.rkt")

(define (WELCOME) "Welcome to RELDB \n >> ")
(define (PROMPT) "\n>> ")
(define (ERROR_INPUT)  "Unknown command: ")
(define (ERROR_ARGUMENTS) "Unsuficient number of arguments or wrong command.\n")
(define (WRONG_TABLE) "Wrong table selected.\n")


(define prompt-read (lambda (Prompt)
                      (newline)
                      (display Prompt)    
                      (read-line))
)

(define manageCommand (lambda (db command )          
                  (let([commandList (regexp-split #px" " command)]);definitions
                             (display "\nCurrent database:") 
                             (display db)
                             (newline)
                             (cond [(equal? command "showall") (display command)(manageCommand db (prompt-read (PROMPT)))] ; if command is showall
                                [(<= (length commandList) 1) (display (ERROR_ARGUMENTS)) (manageCommand db (prompt-read (PROMPT)))];display error
                                [#t (manageCommand (manageCommandAux db commandList) (prompt-read (PROMPT)))]
                            )
                  )
                     )
  )
            
(define manageCommandAux (lambda (db list)
                                (cond
                             [(or (equal? (car list) "addtable") (equal? (car list) "addt")) (addtable db (cdr list))]
                             [(or (equal? (car list) "insert") (equal? (car list) "ins")) (insert db (cdr list))]
                             [#t (display (string-append (ERROR_INPUT) (car list) "\n")) db]
                             ))
)

; Returns index of table in db
(define searchtable (lambda(db tablename [index 0])
                        (cond
                          [(null? db) -1]
                          [(equal? tablename (caaar db)) index]
                          [#t (searchtable (cdr db) tablename (+ 1 index))]
                          )
                        )
)
; Inserts record on a table PD: does not checks length of db vs index of table
(define insertrecord (lambda(db index record)
                       (cond
                         [(> index 0) (cons (car db) (insertrecord (cdr db) (- 1 index) record))]
                         [#t (cons (append (car db) (list record)) (cdr db))]
                         )
                        )
)


(define addtable (lambda(db args)
                          (cond
                            [(= (length args) 1)(display (ERROR_ARGUMENTS)) db]
                            [#t (cons (append (list (car args) 0 0) (list (cdr args))) db)])                        
                        )
)

(define insert (lambda(db args)
                (cond
                  [(equal? (searchtable db (car args)) -1) (display (WRONG_TABLE)) db]
                  [(equal? (length (car (getPos db (searchtable db (car args))))) (length args) ) (insertrecord db (searchtable db (car args)) (cdr args))]
                  )      
                )
)
  
(define (database) (manageCommand '() (prompt-read (WELCOME))));end database

;not
(define (NOT param)(cond[param #f][else #t]))

;returns false if the table has no registers
(define (isEmpty? table)(equal? (cadar table) 0))

;returns n elements
;cars: number of elements to get
;begin: initial position, default=0
(define (carN elements cars [begin 0])(cond
                                        [(> begin 0)(carN (cdr elements) cars (- begin 1))]
                                        [(and(equal? begin 0)(> cars 0)(NOT (equal? '() elements)))(append (list (car elements))(carN (cdr elements) (- cars 1) begin))]
                                        [else null]
                                        )
  )

(define (removeReferenceAux  header foreignKeyCol sourceTableName [nL '()])(cond
                                                                    [(NOT(null? header))
                                                                     (cond
                                                                       [(and (list? (car header))(equal? foreignKeyCol (caar header))(equal? sourceTableName (cadar header)))
                                                                        (append (list #t) nL (list (caar header))(cdr header))
                                                                        ]
                                                                       [else 
                                                                        (removeReferenceAux (cdr header) foreignKeyCol sourceTableName (append nL (list (car header))))
                                                                        ]
                                                                       )
                                                                     ]
                                                                    [else
                                                                     (append (list #f) nL)
                                                                     ]
                                                                   )
  )

(define (removeReference db tableToReference foreignKeyCol sourceTableName) (cond
                                                             [(and (NOT(isEmpty? searchtable(db tableToReference)))(NOT(equal? -1 searchtable(db sourceTableName)))(equal? #t (car (removeReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName))))
                                                              ;TODO se debe sustituir la tabla vieja
                                                              ;La siguiente linea devuelve el header completo
                                                              ;(append  (carN searchtable(db tableToReference) 2)(- 1 (caddar searchtable(db tableToReference)))(car (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName)))
                                                              ]
                                                             )
)


;setReference: aux function
;header is caaaar headerP (just the names)
;it returns a list with a bool as first element
(define (setReferenceAux header foreignKeyCol sourceTableName [nL '()])( cond
                                                                          [(NOT(null? header))
                                                                           (cond
                                                                             [(equal? (car header) foreignKeyCol)
                                                                              (append (list #t) nL (list (append (list (car header)) (list sourceTableName))) (cdr header))
                                                                              ]
                                                                             [else 
                                                                              (setReferenceAux (cdr header) foreignKeyCol sourceTableName (append nL (list (car header))))
                                                                              ]
                                                                             )
                                                                           ]
                                                                          [else 
                                                                           (append (list #f) nL)
                                                                           ]
                                                                          )
  )

;setReference: main function
(define (setReference db tableToReference foreignKeyCol sourceTableName) (cond
                                                             [(and (NOT(isEmpty? searchtable(db tableToReference)))(NOT(equal? -1 searchtable(db sourceTableName)))(equal? #t (car (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName))))
                                                              ;TODO se debe sustituir la tabla vieja
                                                              ;La siguiente linea devuelve el header completo
                                                              ;(append  (carN searchtable(db tableToReference) 2)(+ 1 (caddar searchtable(db tableToReference)))(car (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName)))
                                                              ]
                                                             )
)

;(database)