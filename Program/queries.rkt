#lang scheme
(require "utilities.rkt" "DatabaseUtils.rkt")
(provide query)

(define query (lambda (db args)
                (cond
                [(= 1 (length args) ) (printTable(searchtableget (cdr db) (car args))) db ]
                [#t (queryPrintRows (searchtableget (cdr db) (car args)) (cdr args))]
                )
             )
  )

(define queryPrintRows (lambda (table args)
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
                  (display args)
                  (newline)
                  (display "Records: " )
                  (printQueryRecords (cddar table) (cdr table) args)

                         )
  )
(define (printQueryRecords header records args)
                 (cond
                    [(null? records ) 0]
                    [#t (newline) (printQueryRecord header (car records) args) (printQueryRecords header (cdr records) args)]                      
                  )
  )
(define (printQueryRecord header record args)
  (cond
                    [(null? record) 0]
                    [(in? args (car header)) (display (car record)) (printQueryRecord (cdr header) (cdr record) args)]
                    [#t (printQueryRecord (cdr header) (cdr record) args)]                      
                  )
  )