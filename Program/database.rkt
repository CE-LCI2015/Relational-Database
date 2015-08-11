#lang scheme
(define WELCOME ("Welcome to myDatabase \n >> "))
(define PROMPT ("\n>> "))
(define ERROR_INPUT ("Unknown command: "))
(define ERROR_ARGUMENTS ("Unsuficient number of arguments or wrong command"))

(define prompt-read (lambda (Prompt)
                  (display "\nCurrent database:")  
                  (newline)
                  (display Prompt)
                           
                  (read-line))
)

(define manageCommand (lambda (command)
                           (                           
                            let([commandList (regexp-split #px" " command)]);definitions
                             (if (equal? command "showall") (display command) ; if command is showall
                                (if (<= (length commandList) 1)
                            (display ERROR_ARGUMENTS);display error
                            (manageCommandAux commandList)
                            ))
                           (manageCommand (prompt-read PROMPT))
                                                          );end let
                           )
  )
            


(define manageCommandAux (lambda (list)
                                (cond
                             [(or (equal? (car list) "addtable") (equal? (car list) "addt")) (addtable (cdr list))]
                             [#t (display (string-append ERROR_INPUT (car list)))]
                             )
                                
                              ))

(define addtable (lambda(args db)
                        (let ([num (length args)])
                          (cond
                            [(= num 1)(display ERROR_ARGUMENTS)]
                            [#t (cons args db)])
                          );end let
                        ))

(define (length list)
  (cond[(null? list) 0]
       [#t (+ 1 (length (cdr list)))])
  )
  
(define database (manageCommand (prompt-read WELCOME)));end database


(database)
