
#lang scheme
(require "utilities.rkt")

(define (WELCOME) "Welcome to myDatabase \n >> ")
(define (PROMPT) "\n>> ")
(define (ERROR_INPUT)  "Unknown command: ")
(define (ERROR_ARGUMENTS) "Unsuficient number of arguments or wrong command")



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
                             [#t (display (string-append (ERROR_INPUT) (car list)))]
                             ))
)

(define addtable (lambda(db args)
                        (let ([num (length args)])
                          (cond
                            [(= num 1)(display (ERROR_ARGUMENTS))]
                            [#t (cons args db)])
                          );end let
                        )
)


  
(define (database) (manageCommand '() (prompt-read (WELCOME))));end database

(database)
