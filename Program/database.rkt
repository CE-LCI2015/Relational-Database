
#lang scheme
(require "utilities.rkt")
(require "update.rkt")
(require "saveProcedure.rkt")
(require "DatabaseUtils.rkt")
;;;Database
;
;((PROC)(TABLE1)(TABLE2)...(TABLEN))
;
;;;HEADER
;
;((nombre numeroDeLlavesForaneas columnas.....) registros...)
;;;;

(define (createProceduresList)( list '(("addtable")) '() ) )

(define (manageCommand db command )(cond [(equal? command "showall") (showall (cdr db))(manageCommand db (prompt-read (PROMPT)))] ; if command is showall
                                         [(equal? command "exit") 0]
                                         [(<= (length (regexp-split #px" " command)) 1) (display (ERROR_ARGUMENTS)) (manageCommand db(prompt-read (PROMPT)))];display error
                                         [#t manageCommand (manageCommandAux db (regexp-split #px" " command)) (prompt-read (PROMPT))]
                                         )
(define manageCommand (lambda (db command )          
                             (newline)
                             (cond [(equal? command "showall") (showall (cdr db))(manageCommand db (prompt-read (PROMPT)))] ; if command is showall
                                   [(equal? command "exit") 0]
                                [(<= (length (regexp-split #px" " command)) 1) (display (ERROR_ARGUMENTS)) (manageCommand db(prompt-read (PROMPT)))];display error
                                [#t (manageCommand (manageCommandAux db (split command)) (prompt-read (PROMPT)))]
                            )                  
                     )
  )
            
(define (manageCommandAux db list)(cond
                                    [(or (equal? (car list) "addtable") (equal? (car list) "addt")) (addtable db (cdr list))]
                                    [(or (equal? (car list) "insert") (equal? (car list) "ins")) (insert db (cdr list))]
                                    [(or (equal? (car list) "update") (equal? (car list) "ud")) (update db (cdr list))]
                                    [(equal? (car list) "cproc") (cproc db (cdr list))]
                                    [(equal? (car list) "eval") (ev db (cdr list))]
                                    [#t (display (string-append (ERROR_INPUT) (car list) "\n")) db]
                                    )
(define manageCommandAux (lambda (db args)
                                (cond
                             [(or (equal? (car args) "addtable") (equal? (car args) "addt")) (addtable db (cdr args))]
                             [(or (equal? (car args) "insert") (equal? (car args) "ins")) (insert db (cdr args))]
                             [(or (equal? (car args) "update") (equal? (car args) "ud")) (update db (cdr args))]
                             [(equal? (car args) "cproc") (cproc db (cdr args))]
                             ;[(equal? (car args) "eval") (evalTEC db (cdr args))]
                             [#t (display (string-append (ERROR_INPUT) (car args) "\n")) db]
                             ))
)

(define (addtable db args)(cond
                            [(= (length args) 1)(display (ERROR_ARGUMENTS)) db]
                            [#t (cons (car db) (cons (list (append (list (car args) 0) (cdr args))) (cdr db)))]) ;adds table with header 0 for foreign keys                       
                        )
)
(define insert (lambda(db args)
                (cond
                  [(equal? (searchtable (cdr db) (car args)) -1) db]; error wrong table
                  [(equal? (length (cddar (searchtableget (cdr db) (car args)))) (length (cdr args)) ) (cons (car db) (insertrecord (cdr db) (car args) (cdr args)))];Insert record if lengths are equal
                  [#t (display (ERROR_ARGUMENTS)) db]
                  )      
                )
)
  
(define (database procedures) (manageCommand (list procedures '()) (prompt-read (WELCOME))));end database

(database (createProceduresList))
