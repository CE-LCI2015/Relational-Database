#lang scheme
(require "utilities.rkt")
(provide cproc)

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
                                                        [(equal? (car functions) (car meaning))
                                                         (cons (cons word meaning) dictionary)
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




(define (cproc db args)(
                              append (list (list (append (caar db)) (saveProcedure (caar db) (cadar db) (car args) (cdr args)))) (cdr db)

                        )
  )

