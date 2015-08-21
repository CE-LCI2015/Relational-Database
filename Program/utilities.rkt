#lang scheme
(provide split or length)
(provide length)
(provide getIndex)
(provide getPos)
(provide in?)
(provide NOT)
(provide join)
(provide joinreverse)
(provide WELCOME)
(provide PROMPT)
(provide ERROR_INPUT)
(provide ERROR_ARGUMENTS)
(provide WRONG_TABLE)
(provide WRONG_PK PK_INVALID)
(provide carN)


(define (WELCOME) "Welcome to RELDB \n>> ")
(define (PROMPT) "\n>> ")
(define (ERROR_INPUT)  "Unknown command: ")
(define (ERROR_ARGUMENTS) "Wrong or unsuficient arguments.\n")
(define (WRONG_TABLE) "Wrong table selected.\n")
(define (WRONG_PK) "PrimaryKey does not exist.\n")
(define (PK_INVALID) "PrimaryKey already exists.\n")
;;Defining or
(define (or . args)
  (cond
    [(null? args) #f]
    [(car args) #t]
    [#t (oraux (cdr args))]
    )
  )
(define (oraux args)
  (cond
    [(null? args) #f]
    [(car args) #t]
    [#t (oraux (cdr args))]
    )
  )

;;Defining and
(define (and . args)
  (cond
    [(null? args) #t]
    [(car args) (andaux args)]
    [#t #f]
    )
  )
(define (andaux args)
  (cond
    [(null? args) #t]
    [(car args) (andaux (cdr args))]
    [#t #f]
    )
  )



(define (cdrSTR string [bool #f])
  (cond
    [(= (string-length string) 0) null]
    [(and (equal? (string-ref string 0) #\())(cdrSTR (substring string 1) #t )]
    [(and (equal? (string-ref string 0) #\))) (substring string 1)]
    [(and (equal? (string-ref string 0) #\space) (equal? #f bool)) (substring string 1)] 
    [(and (equal? (string-ref string 0) #\space) (equal? #t bool)) (cdrSTR (substring string 1) #t)] 
    [else (cdrSTR (substring string 1) #f)]
    )
  )

(define (carSTRAux string [bool #f] [elem null])
                                              (cond
                                                [(equal? (string-length string) 0)elem]
                                                [(and (equal? (string-ref string 0) #\() (null? elem))(carSTRAux (substring string 1) #t )]
                                                [(and (equal? (string-ref string 0) #\))) elem]
                                                [(and (equal? (string-ref string 0) #\space) (equal? #f bool)) elem] 
                                                [(and (equal? (string-ref string 0) #\space) (equal? #t bool)) (append elem (carSTRAux (substring string 1) #t elem))] 
                                                [else (append elem (list (make-string 1 (string-ref string 0))) (carSTRAux (substring string 1) #f elem))]
                                                )
  )

(define (carSTR string)(carSTRAux string))

(define (split string)
  (regexp-split #px" " string)
  )
;Element counter
(define (length lis)
  (cond
	[(NOT(null? lis)) (+ 1 (length (cdr lis)))]
        [else 0]
)
)

;Returns the element of the specified index
(define (getElement lis index)(
                                if (equal? index 0) 
                                   (car lis)
                                   (getIndex (cdr lis) (- index 1))
                               )
 )

;Evaluates the index and returns the element of the specified index
(define (getIndex lis index)(
                                if (< index (length lis)) 
                                   (getElement lis index)
                                   "Index out of bound."
                               )
 )
;Efficient getIndex
(define (getPos L position)
  (cond [(null? L) (error "Index out of bounds")]
    [(= 0 position)(car L)]
    [#t (getPos (cdr L) (sub1 position))]
    )
  )
;Element in list?
(define (in? L element)
  (cond [(null? L) #f]
    [(equal? element (car L))#t]
    [#t (in? (cdr L) element)]
    )
  )

(define (carN lis f [i 0])(cond
                              [(NOT(null? lis))
                               (cond
                                 [(> i 0)(carN (cdr lis) f (- i 1))]
                                 [(or (> f i)(= f i))(append (car lis) (carN (cdr lis) (- f 1) i))]
                               )
                              ]
                              [else null]
                              )
  )

; NOT redefiniton
(define (NOT param)(cond[param #f][else #t]))


;join strings on a list
(define (join L)(cond [(null? L) ""][#t (string-append (car L) " " (join (cdr L)))]))
;join strings on a reversed list
(define (joinreverse L)(cond [(null? L) ""][#t (string-append  (join (cdr L)) " " (car L) )]))