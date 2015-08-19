#lang scheme
(require "utilities.rkt")
(provide cproc ev)
(define (saveProcedure functions dictionary word meaning)(cond
                                                     [(NOT(null? functions))
                                                      (cond
                                                        [(equal? (caar functions) (car meaning))
                                                         (append dictionary (list (list word meaning)))
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
                                           [(and (equal? (caaar functions) alias) (equal? (length params)(length (cadaar functions))))
                                            (display (append (list (caadar functions)) db params))
                                            (eval (append (list (caadar functions)) db params))
                                            ]
                                           [else (evAux (cdr functions) alias params)]
                                           )
                                         ]
                                        [else 
                                         (display "Error, procedure not found")
                                         ]
                                       )
  )

(define (cproc db args)(cond
                             ;[(and (> (length args) 5) (list? (cdr args))(list? (cddddr args)))
                              (append (list (caar db) (saveProcedure (caar db) (cadar db) (car args) (cdr args))) (cdr db))
                              ;]
                             ;[else 
                              ;(display "Parameter fault")
                              ;db
                              ;]
                        )
  )

(define (ev db args)((evAux db (caar db) (car args) (cdr args))))