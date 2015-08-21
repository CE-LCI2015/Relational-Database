#lang scheme
(require "utilities.rkt")

(provide prompt-read showall searchtable searchtableget searchpk insertrecord remover printTable deltable)

(define prompt-read (lambda (Prompt)
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
                  (newline)
                  (display "-----------------------" )
                  (newline)
                  (display "Table Name: " )
                  (display (caar table))
                  (newline)
                  (display "Foreign Keys: " )
                  (display (cadar table))
                  (newline)
                  (display "Columns: " )
                  (newline)
                  (display (cddar table))
                  (newline)
                  (display "Records: " )
                  (printRecords  (cdr table))
                  )
  )
(define printRecords( lambda (records)
                       (cond
                    [(null? records ) 0]
                    [#t (newline) (display (car records)) (printRecords (cdr records))]                      
                       )
                    )
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
                          [#t (searchtableget (cdr db) tablename (+ 1 index))]
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
                          [#t (searchpkaux (cdr table) primarykey (+ 1 index))]
                          )
                   )
  
)
; Inserts record on a table PD: does not checks length of db vs index of table
(define insertrecord (lambda(db tablename record)
                       (cond
                         [(null? db) (display (WRONG_TABLE)) db]
                         [(equal? tablename (caaar db))
                          (cond
                            [(NOT (= (searchpkaux (cdar db) (car record)) -1) ) (display (PK_INVALID)) db]
                            [#t (cons (cons (caar db) (cons record (cdar db))) (cdr db))]
                            )
                          ]
                         [#t (cons (car db) (insertrecord (cdr db) tablename record))]
                         )
                        )
)


(define deltable (lambda(db args)
                   (cond
                  [(NOT (= (length args) 1)) (display (ERROR_ARGUMENTS)) db]  ; wrong arguments
                  [#t  (cons (car db) (delaux (cdr db) args))]
                   )
                   )
  )
;findtable to delete
(define delaux (lambda(db args)
                     (cond
                      [(NOT(equal? (car args) (caaar db))) (cons (car db) (delaux (cdr db) args))]
                      [(> (length (cdar db)) 0) (display "Error table not empty") db]
                      [#t  (cdr db) ]
                      )     
                     )
  )
(define remover (lambda(db args)
                  (cond
                  [(NOT (= (length args) 2)) (display (ERROR_ARGUMENTS)) db]  ; wrong arguments               
                  [(equal? (searchpk (cdr db) (car args) (cadr args)) -1)   db ] ; register not found
                  [#t  (cons (car db) (removeraux (cdr db) args))]
                  )
               )
  )
;searches table
(define removeraux (lambda(db args)
                     (cond
                      [(NOT(equal? (car args) (caaar db))) (cons (car db) (removeraux (cdr db) args))]
                      [#t (cons (cons (caar db) (removeraux2  (cdar db) (cdr args))) (cdr db) )]
                      )     
                     )
  )

;select record
(define removeraux2 (lambda(table args)(cond
                                         [(NOT(equal? (car args) (caar table))) (cons (car table) (removeraux2 (cdr table) args))]
                                         [#t (cdr table)]
                                         )
                      )
  )