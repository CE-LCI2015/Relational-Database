#lang scheme

(define database (lambda ()



        ; These definitions are local to database
        (letrec

            ; These strings are used as prompts
           ((WELCOME "Welcome to myDatabase \n >> ")
            (PROMPT "\n>> ")
            (ERROR_INPUT "Unknown command")

            ; This function displays a prompt then returns
            ; a value read.
            (prompt-read (lambda (Prompt)

                  (display Prompt)
                  (read)))


            

            (transaction (lambda (command)
                           (cond
                             [(or (equal? command "addtable") (equal? command "addt"))] (display "addtable")
                             [#t](display ERROR_INPUT)
                             )
                          
                           (transaction (prompt-read PROMPT)))))

  (transaction (prompt-read WELCOME)))))



(database)