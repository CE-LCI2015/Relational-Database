#lang scheme
(require "utilities.rkt")
(provide cproc ev)

(define (wifs str)(cond
                    [(string? str) (list (with-input-from-string str (lambda() (read))))]
                    [(list? str)(cond
                                  [(NOT(null? str))(append (wifs (car str))(wifs (cdr str)))]
                                  [else null]
                                  )
                     ]
                    )
  )
(define (saveProcedure functions dictionary word meaning)(cond
                                                     [(NOT(null? functions))
                                                      (cond
                                                        [(equal? (caar functions) (car meaning))
                                                         (append dictionary (list (append (list word) meaning)))
                                                         ]
                                                        [else (saveProcedure (cdr functions) dictionary word meaning)]
                                                        )
                                                      ]
                                                     [else 
                                                      (display "\nError, procedure not found")
                                                      dictionary
                                                      ]
                                                     )
)

(define (evAux db functions alias params)(cond
                                        [(NOT(null? functions))
                                         (cond
                                           [(equal? (caar functions) alias)
                                            (display (eval (append (wifs(cadar functions)) (list db) (wifs params))))
                                            db
                                            ]
                                           [else (evAux db (cdr functions) alias params)]
                                           )
                                         ]
                                        [else 
                                         (display "Error, procedure not found")
                                         db
                                         ]
                                       )
  )

(define (cproc db args)(;cond
                             ;[(and (> (length args) 5) (list? (cdr args))(list? (cddddr args)))
                              append (list (list (append (caar db)) (saveProcedure (caar db) (cadar db) (car args) (cdr args)))) (cdr db)
                              ;]
                             ;[else 
                              ;(display "Parameter fault")
                              ;db
                              ;]
                        )
  )

(define (ev db args)(cons (car db) (cons (list (evAux db (cadar db) (car args) (cdr args))) (cdr db))))