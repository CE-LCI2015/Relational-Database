
#lang scheme
(require "utilities.rkt" "update.rkt" "saveProcedure.rkt" "DatabaseUtils.rkt" "queries.rkt")

;;;Database
;
;((PROC)(TABLE1)(TABLE2)...(TABLEN))
;
;;;HEADER
;
;((nombre numeroDeLlavesForaneas columnas.....) registros...)
;;;;
;;All posiblie proceadures
(define (createProceduresList)( list '("addtable" "addt" "insert" "ins" "update" "ud" "remover" "rr" "deltable" "dt" "query" "showall") '() ) )
;Main recursion where commands happen
(define (manageCommand db command)(
  cond [(equal? command "showall") (showall (cdr db))(manageCommand db (prompt-read (PROMPT)))] ; if command is showall
  [(equal? command "exit") (exit)]
  [(<= (length (regexp-split #px" " command)) 1) (display (ERROR_ARGUMENTS)) (manageCommand db(prompt-read (PROMPT)))];display error
  [#t (manageCommand (manageCommandAux db (split command)) (prompt-read (PROMPT)))]
  )
)                  
                     
 
    ;auxiliar to command manager        
(define manageCommandAux (lambda (db args);(display db)
                                (cond
                             [(or (equal? (car args) "addtable") (equal? (car args) "addt")) (addtable db (cdr args))]
                             [(or (equal? (car args) "insert") (equal? (car args) "ins")) (insert db (cdr args))]
                             [(or (equal? (car args) "update") (equal? (car args) "ud")) (update db (cdr args))]
                             [(or (equal? (car args) "remover") (equal? (car args) "rr")) (remover db (cdr args))]
                             [(or (equal? (car args) "deltable") (equal? (car args) "dt")) (deltable db (cdr args))]
                             [(equal? (car args) "query") (query db (cdr args)) db]
                             [(equal? (car args) "cproc") (cproc db (cdr args)) ]
                             [(equal? (car args) "eval") (ev db (cdr args)) ]
                             [#t (display (string-append (ERROR_INPUT) (car args) "\n")) db]
                             ))
)
;Eval fucntion
(define (ev db args)
  (evAux db (cadar db) (car args) (cdr args))
  )
;finds associated command
(define (evAux db functions alias params)(cond
                                        [(NOT(null? functions))
                                         (cond
                                           [(equal? (caar functions) alias)
                                            (manageCommandAux db (append (cdar functions) params))
                                            ]
                                           [else (evAux db (cdr functions) alias params)]
                                           )
                                         ]
                                        [else 
                                         (display "Error, procedure not found")
                                         db
                                         ]
                                       )
  )



  
  
;Adds a table in the db
(define (addtable db args)(cond
                            [(= (length args) 1)(display (ERROR_ARGUMENTS)) db]
                            [#t (cons (car db) (cons (list (append (list (car args) 0) (cdr args))) (cdr db)))]) ;adds table with header 0 for foreign keys                       
                        )
;Insert a record
(define insert (lambda(db args)
                (cond
                  [(equal? (searchtable (cdr db) (car args)) -1) db]; error wrong table
                  [(equal? (length (cddar (searchtableget (cdr db) (car args)))) (length (cdr args)) ) (cons (car db) (insertrecord (cdr db) (car args) (cdr args)))];Insert record if lengths are equal
                  [#t (display (ERROR_ARGUMENTS)) db]
                  )      
                )
)
;Main
(define (database) (manageCommand (list (createProceduresList)) (prompt-read (WELCOME))));end database

(database )
