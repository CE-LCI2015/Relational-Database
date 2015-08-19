#lang scheme
(require "utilities.rkt")

(provide prompt-read showall searchtable searchtableget searchpk insertrecord)

(define prompt-read (lambda (Prompt)
                  (newline)
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
                  (display "Columns: " )
                  (display (cddar table))
                  (newline)
                  (display "Registers: " )
                  (display  (cdr table))
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
                          [#t (searchtable (cdr db) tablename (+ 1 index))]
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
                          [#t (searchpk (cdr table) primarykey (+ 1 index))]
                          )
                   )
  
)
; Inserts record on a table PD: does not checks length of db vs index of table
(define insertrecord (lambda(db tablename record)
                       (cond
                         [(null? db) (display (WRONG_TABLE)) db]
                         [(equal? tablename (caaar db)) (cons (cons (caar db) (cons record (cdar db))) (cdr db))]
                         [#t (cons (car db) (insertrecord (cdr db) tablename record))]
                         )
                        )
)

