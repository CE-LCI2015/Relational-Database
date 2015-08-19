#lang scheme
(require "utilities.rkt")

(define (getFunctions)(append (list (append '(power) '((x n)))) (list (append '(fact) '((n))))))

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

(define (power x n)((cond[(> n 0)(* x (power x (- n 1)))][else 1])))
(define (fact n)((cond[(> n 1)(* n (fact (- n 1)))][else 1])))

;(saveProcedure '() (getFunctions) '(f(n)) '(fact (n)))
;(saveProcedure '() (getFunctions) '(p(n)) '(power (x n)))
(saveProcedure 
 (saveProcedure '() (getFunctions) '(f(n)) '(fact (n)))
 (getFunctions)
 '(p(x n)) 
 '(power (x n))
)