#lang scheme
(require "utilities.rkt")

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
                                                             [(and (NOT(isEmpty? searchtable(db tableToReference)))(NOT(equal? -1 searchtable(db sourceTableName)))(equal? #t (car (removeReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName))))
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
(define (setReference db tableToReference foreignKeyCol sourceTableName) (cond
                                                             [(and (NOT(isEmpty? searchtable(db tableToReference)))(NOT(equal? -1 searchtable(db sourceTableName)))(equal? #t (car (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName))))
                                                              ;TODO se debe sustituir la tabla vieja
                                                              ;La siguiente linea devuelve el header completo
                                                              ;(append  (carN searchtable(db tableToReference) 2)(+ 1 (caddar searchtable(db tableToReference)))(car (setReferenceAux (cadddr (cdr (car searchtable(db tableToReference)))) foreignKeyCol sourceTableName)))
                                                              ]
                                                             )
)

