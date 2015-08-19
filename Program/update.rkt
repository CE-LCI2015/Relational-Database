#lang scheme
(require "utilities.rkt")
(require "DatabaseUtils.rkt")
(provide update)

;Command for update register
(define update (lambda(db args)
                (cond
                  [(< (length args) 3) (display (ERROR_ARGUMENTS)) db]                 
                  [(equal? (searchpk db (car args) (cadr args)) -1)   db ] ; register not found
                  [(null? (updateargs (cdr(cdddar (searchtableget db (car args)))) (cddr args)))  db]; error with command
                  [#t  (updateaux db args (updateargs (cdr(cdddar (searchtableget db (car args)))) (cddr args)))]
                  )      
                )
)
;select table
(define updateaux (lambda(db args updateargs)
                    (cond
                      [(NOT(equal? (car args) (caaar db))) (cons (car db) (updateaux (cdr db) args update args))]
                      [#t (cons (cons caar (updateaux2 (cdaar db) (cdar db) (cdr args) updateargs)) (cdr db) )];fix header
                      )                 
                 )
  )
;select primarykey
(define updateaux2 (lambda(header table args updateargs)
                                  (cond
                      [(NOT(equal? (car args) (car table))) (cons (car table) (updateaux2 header (cdr table) args update args))]
                      [#t (cons (updateaux3 header (car table) updateargs) (cdr table))]
                      )
                 )
  )
;change data from record
(define updateaux3 (lambda(header record updateargs)
                                  (cond
                      [(null? record) '()]
                      [#t (cons (updatenamevalue (car header) (car record) updateargs) (updateaux3 (cdr header) (cdr record) updateargs))]
                      )
                 )
  )
;Compares a row name and returns value to assign
(define updatenamevalue (lambda(rowname original options)
                      (cond
                      [(null? options) original]
                      [(equal? rowname (car options)) (cadr options)]
                      [#t (updatenamevalue rowname original (cdr options))]
                      )
                 )
  )

;analize update arguments
(define updateargs (lambda(header args [result '()] [gotHeader? #f])
                (cond
                  [(null? args) (cond
                                  [(null? result)  result]
                                  [(null? (cdar result)) (display (ERROR_ARGUMENTS)) '()]
                                  [#t (cons (list (caar result) (joinreverse (cdar result)))(cdr result))]
                                  )]
                  [(in? header (car args))
                   (cond
                     [gotHeader? (cond
                                   [(null? (cdar result))(display (ERROR_ARGUMENTS)) '()]
                                   [#t (updateargs header (cdr args) (cons (list (car args)) (cons (list (caar result) (joinreverse (cdar result)))(cdr result))) #t)]
                                   )                                 
                     ]
                     [#t (updateargs header (cdr args) (cons (list (car args)) result) #t)])
                   ]
                  [#t (cond
                        
                        [gotHeader? (updateargs header (cdr args) (cons (append (list (caar result) (car args)) (cdar result)) (cdr result)) #t)]
                        [#t (display (ERROR_ARGUMENTS)) '()]
                        )
                      ]
                )
        )
)