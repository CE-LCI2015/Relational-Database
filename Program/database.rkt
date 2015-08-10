#lang scheme
(define (length list)
  (cond[(null? list) 0]
       [#t (+ 1 (length (cdr list)))])
  )
(define database (lambda ()



        ; These definitions are local to database
        (letrec

            ; These strings are used as prompts
            (;let definitions-begin
            (WELCOME "Welcome to myDatabase \n >> ")
            (PROMPT "\n>> ")
            (ERROR_INPUT "Unknown command: ")
            (ERROR_ARGUMENTS "Unsuficient number of arguments or wrong command")

            ; This function displays a prompt then returns
            ; a value read.
            (prompt-read (lambda (Prompt)

                  (display Prompt)
                  (read-line)))


            

            (manageCommand (lambda (command)
                           (                           
                            let([commandList (regexp-split #px" " command)]);definitions
                             (if (equal? command "showall") (display command) ; if command is showall
                                (if (<= (length commandList) 1)
                            (display ERROR_ARGUMENTS);display error
                            (manageCommandAux commandList)
                            ))
                            
                           (manageCommand (prompt-read PROMPT))
                             
                                                          );end let
                           
                           ))
            (manageCommandAux (lambda (list)
                                (cond
                             [(or (equal? (car list) 'addtable) (equal? (car list) 'addt)) (display 'addtable)]
                             [#t (display (string-append ERROR_INPUT (car list)))]
                             )
                                
                              ))
            
            
            );let definitions-end

  (manageCommand (prompt-read WELCOME)));end let
                   )
  );end database


(database)