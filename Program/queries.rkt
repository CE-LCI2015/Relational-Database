#lang scheme
(require "utilities.rkt" "DatabaseUtils.rkt")
(provide query)

(define query (lambda (db args)
                (printTable(searchtableget (cdr db) (car args)))
                db
                )
  )