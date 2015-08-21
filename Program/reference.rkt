#lang scheme
(require "utilities.rkt" "DatabaseUtils.rkt")
(provide removeReference setReference)

(define (removeReferenceAux  header foreignKeyCol sourceTableName [nL '()])(cond
                                                                    [(NOT(null? header))
                                                                     (cond
                                                                       [(and (list? (car header))(equal? foreignKeyCol (caar header))(equal? sourceTableName (cadar header)))
                                                                        (append (list #t) nL (list (caar header))(cdr header))
                                                                        ]
                                                                       [else
                                                                        (removeReferenceAux (cdr header) foreignKeyCol sourceTableName (append nL (list (car header))))
                                                                        ]
                                                                       )
                                                                     ]
                                                                    [else
                                                                     (append (list #f) nL)
                                                                     ]
                                                                   )
  )

(define (removeReference db tableToReference foreignKeyCol sourceTableName) (cond
                                                             [(and (NOT(null? (cdr (searchtableget db tableToReference))))(NOT(equal? -1 (searchtableget db sourceTableName)))(equal? #t (car (removeReferenceAux (cadddr (cdr (car (searchtableget db tableToReference)))) foreignKeyCol sourceTableName))))
                                                              ;TODO se debe sustituir la tabla vieja
                                                              ;La siguiente linea devuelve el header completo
                                                              ;(append  (carN searchtable(db tableToReference) 2)(- 1 (caddar searchtable(db tableToReference)))(car (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName)))
                                                              ]
                                                             )
)


;setReference: aux function
;header is caaaar headerP (just the names)
;it returns a list with a bool as first element
(define (setReferenceAux header foreignKeyCol sourceTableName [nL '()])( cond
                                                                          [(NOT(null? header))
                                                                           (cond
                                                                             [(equal? (car header) foreignKeyCol)
                                                                              (append (list #t) nL (list (append (list (car header)) (list sourceTableName))) (cdr header))
                                                                              ]
                                                                             [else
                                                                              (setReferenceAux (cdr header) foreignKeyCol sourceTableName (append nL (list (car header))))
                                                                              ]
                                                                             )
                                                                           ]
                                                                          [else
                                                                           (append (list #f) nL)
                                                                           ]
                                                                          )
  )

;setReference: main function
(define (setReference db args) (cond
                                 [(and (NOT(equal? -1 (searchtableget (cdr db) (cdddr args))))(NOT(null? (cdr (searchtableget (cdr db) (cadr args)))))(equal? #t (car (setReferenceAux (cadddr (cdr (car (searchtableget (cdr db) (cadr args))))) (caddr args)(cadddr args)))))
                                  append (car db) (carN (cdr db) (- (searchtable (cdr db) (cadr args)) 1)) (setReferenceAux (car (searchtableget (cdr db) (cadr args))) (cadddr args) (cadddr args)) (+ (searchtable (cdr args)) 1)
                                  ]
                                 )
  )

