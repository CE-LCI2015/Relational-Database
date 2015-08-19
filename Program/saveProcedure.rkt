#lang scheme
(require "utilities.rkt")

;TMPS
(define (power x n)(cond[(> n 0)(* x (power x (- n 1)))][else 1]))
(define (fact n)(cond[(> n 1)(* n (fact (- n 1)))][else 1]))
(define (getFunctions)(append (list (append '(power) '((x n)))) (list (append '(fact) '((n))))))
(define (getDictionaryTMP)(saveProcedure (saveProcedure '() (getFunctions) '(f(n)) '(fact (n))) (getFunctions) '(p(x n)) '(power (x n))))
;~TMPS

(define (saveProcedure lis functions word meaning)(cond
                                                     [(NOT(null? functions))
                                                      (cond
                                                        [(equal? (car functions) meaning)(append lis (list (list word meaning)))]
                                                        [else (saveProcedure lis (cdr functions) word meaning)]
                                                        )
                                                      ]
                                                     [else "Error, procedure not found"]
                                                     )
)

(define (evalF functions alias params)(cond
                                        [(NOT(null? functions))
                                         (cond
                                           [(and (equal? (caaar functions) alias) (equal? (length params)(length (cadaar functions))))
                                            (eval (append (list (caadar functions)) params))
                                            ]
                                           [else (evalF (cdr functions) alias params)]
                                           )
                                         ]
                                        [else "Error, procedure not found"]
                                       )
  )


;(evalF (getDictionaryTMP) 'f '(3))
;(evalF (getDictionaryTMP) 'p '(2 3))