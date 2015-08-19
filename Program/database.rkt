
#lang scheme
(require "utilities.rkt")
(require "update.rkt")
(define (WELCOME) "Welcome to RELDB \n >> ")
(define (PROMPT) "\n>> ")
(define (ERROR_INPUT)  "Unknown command: ")
(define (ERROR_ARGUMENTS) "Wrong or unsuficient arguments.\n")
(define (WRONG_TABLE) "Wrong table selected.\n")
(define (WRONG_PK) "PrimaryKey does not exist.\n")

(define prompt-read (lambda (Prompt)
                  (newline)
                  (display Prompt)    
                  (read-line))
)
(define showall (lambda (db)
                  (cond
                    [(null? db ) 0]
                    [#t (printTable (car db)) (showall (cdr db))]
                    )
                  )
  )
(define printTable (lambda (table)
                  (display "HEADER: " )
                  (display (car table))
                  (newline)
                  (display (cdr table))
                  (newline)
                  )
  )
(define manageCommand (lambda (db command )          
                             (newline)
                             (cond [(equal? command "showall") (showall db)(manageCommand db (prompt-read (PROMPT)))] ; if command is showall
                                   [(equal? command "exit") 0]
                                [(<= (length (regexp-split #px" " command)) 1) (display (ERROR_ARGUMENTS)) (manageCommand db (prompt-read (PROMPT)))];display error
                                [#t (manageCommand (manageCommandAux db (regexp-split #px" " command)) (prompt-read (PROMPT)))]
                            )                  
                     )
  )
            
(define manageCommandAux (lambda (db list)
                                (cond
                             [(or (equal? (car list) "addtable") (equal? (car list) "addt")) (addtable db (cdr list))]
                             [(or (equal? (car list) "insert") (equal? (car list) "ins")) (insert db (cdr list))]
                             [(or (equal? (car list) "update") (equal? (car list) "ud")) (update db (cdr list))]
                             [#t (display (string-append (ERROR_INPUT) (car list) "\n")) db]
                             ))
)

; Returns index of table in db
(define searchtable (lambda(db tablename [index 0])
                        (cond
                          [(null? db) (display (WRONG_TABLE)) -1]
                          [(equal? tablename (caaar db)) index]
                          [#t (searchtable (cdr db) tablename (+ 1 index))]
                          )
                        )
)
; Returns table in db
(define searchtableget (lambda(db tablename [index 0])
                        (cond
                          [(null? db) (display (WRONG_TABLE)) -1]
                          [(equal? tablename (caaar db)) (car db)]
                          [#t (searchtable (cdr db) tablename (+ 1 index))]
                          )
                        )
)

; Returns index of primarykey in db
(define searchpk (lambda(db tablename primarykey [index 0])
                        (cond
                          [(null? db) (display (WRONG_TABLE)) -1]
                          [(equal? tablename (caaar db)) (searchpkaux (cdar db) primarykey)]
                          [#t (searchtable (cdr db) tablename (+ 1 index))]
                          )
                   )
)

; Returns index of primarykey in db
; table is the table without header
(define searchpkaux (lambda(table primarykey [index 0])
                        (cond
                          [(null? table) (display (WRONG_PK)) -1]
                          [(equal? primarykey (caar table)) index]
                          [#t (searchpk (cdr table) primarykey (+ 1 index))]
                          )
                   )
  
)
; Inserts record on a table PD: does not checks length of db vs index of table
(define insertrecord (lambda(db index record)
                       (cond
                         [(> index 0) (cons (car db) (insertrecord (cdr db) (sub1 index) record))]
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
                  [(equal? (searchtable db (car args)) -1) db]; error wrong table
                  [(equal? (cadar (searchtableget db (car args))) (sub1 (length args)) ) (insertrecord db (searchtable db (car args)) (cdr args))]
                  [#t (display "error") db]
                  )      
                )
)


  
(define (database) (manageCommand '() (prompt-read (WELCOME))));end database



;header is caaar headerP (just the names without the pk)
(define (setReferenceAux header foreignKeyCol sourceTableName)( cond
                                                            [(equal? (car header) foreignKeyCol)
                                                             (append (list (append (list (car header)) (list sourceTableName))) (cdr header))
                                                             ]
                                                            [else 
                                                             (append (list (car header))(setReferenceAux (cdr header) foreignKeyCol sourceTableName))
                                                             ]
                                                            )
  )

(database)
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
                                                              ;(append  (carN searchtable(db tableToReference) 2)(- 1 (caddar searchtable(db tableToReference)))(cdr (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName)))
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
                                                              ;(append  (carN searchtable(db tableToReference) 2)(+ 1 (caddar searchtable(db tableToReference)))(cdr (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName)))
                                                              ]
                                                             )
)
