
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
                                [#t (manageCommand (manageCommandAux db (split command)) (prompt-read (PROMPT)))]
                            )                  
                     )
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

(define cproc (lambda(db args)
                          (display (car db))
                          db                
                        )
  )

(define addtable (lambda(db args)
                          (cond
                            [(= (length args) 1)(display (ERROR_ARGUMENTS)) db]
                            [#t (cons (car db) (cons (list (append (list (car args) 0) (cdr args))) (cdr db)))]) ;adds table with header 0 for foreign keys                       
                        )
)
(define insert (lambda(db args)
                (cond
                  [(equal? (searchtable (cdr db) (car args)) -1) db]; error wrong table
                  [(equal? (length (cddar (searchtableget (cdr db) (car args)))) (length (cdr args)) ) (insertrecord db (searchtable (cdr db) (car args)) (cdr args))];Insert record if lengths are equal
                  [#t (display (ERROR_ARGUMENTS)) db]
                  )      
                )
)


  
(define (database) (manageCommand '(() ) (prompt-read (WELCOME))));end database


(define (databaseExample);; Execute this commands
  (manageCommand (manageCommandAux (manageCommandAux '(() ) (split "addt estudiantes cedula nombre"))  (split "addt profesores id nombre lugar") )  "showall")
  )
(databaseExample)
