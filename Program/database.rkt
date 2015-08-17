
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
                  [(equal? (cadar (getPos db (searchtable db (car args)))) (sub1 (length args)) ) (insertrecord db (searchtable db (car args)) (cdr args))]
                  [#t (display "error") db]
                  )      
                )
)

  
(define (database) (manageCommand '() (prompt-read (WELCOME))));end database

(define (NOT param)(cond[param #f][else #t]))

;header is caar headerP (just the names)
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