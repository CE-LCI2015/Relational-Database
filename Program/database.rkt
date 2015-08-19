
#lang scheme
(require "utilities.rkt")
(require "update.rkt")
(require "DatabaseUtils.rkt")
;;;Database
;
;((PROC)(TABLE1)(TABLE2)...(TABLEN))
;
;;;HEADER
;
;((nombre numeroDeLlavesForaneas columnas.....) registros...)
;;;;

(define manageCommand (lambda (db command )          
                             (newline)
                             (cond [(equal? command "showall") (showall (cdr db))(manageCommand db (prompt-read (PROMPT)))] ; if command is showall
                                   [(equal? command "exit") 0]
                                [(<= (length (regexp-split #px" " command)) 1) (display (ERROR_ARGUMENTS)) (manageCommand db(prompt-read (PROMPT)))];display error
                                [#t (manageCommand (manageCommandAux db (regexp-split #px" " command)) (prompt-read (PROMPT)))]
                            )                  
                     )
  )
            
(define manageCommandAux (lambda (db list)
                                (cond
                             [(or (equal? (car list) "addtable") (equal? (car list) "addt")) (addtable db (cdr list))]
                             [(or (equal? (car list) "insert") (equal? (car list) "ins")) (insert db (cdr list))]
                             [(or (equal? (car list) "update") (equal? (car list) "ud")) (update db (cdr list))]
                             [(equal? (car list) "cproc") (cproc db (cdr list))]
                             [#t (display (string-append (ERROR_INPUT) (car list) "\n")) db]
                             ))
)

(define cproc (lambda(db args)
                          (display (car db))
                          db                
                        )
  )

(define addtable (lambda(db args)
                          (cond
                            [(= (length args) 1)(display (ERROR_ARGUMENTS)) db]
                            [#t (cons (car db) (cons (list (append (list (car args) 0) (cdr args))) (cdr db)))])                        
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


  
(define (database) (manageCommand '(() ) (prompt-read (WELCOME))));end database

(database)
