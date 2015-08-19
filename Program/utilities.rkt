#lang scheme
(provide length)
(provide getElement)
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
(provide WRONG_PK)

(define (WELCOME) "Welcome to RELDB \n >> ")
(define (PROMPT) "\n>> ")
(define (ERROR_INPUT)  "Unknown command: ")
(define (ERROR_ARGUMENTS) "Wrong or unsuficient arguments.\n")
(define (WRONG_TABLE) "Wrong table selected.\n")
(define (WRONG_PK) "PrimaryKey does not exist.\n")



;Element counter
(define (length list)
  (cond[(null? list) 0]
       [#t (+ 1 (length (cdr list)))])
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


; NOT redefiniton
(define (NOT param)(cond[param #f][else #t]))


;join strings on a list
(define (join L)(cond [(null? L) ""][#t (string-append (car L) " " (join (cdr L)))]))
;join strings on a reversed list
(define (joinreverse L)(cond [(null? L) ""][#t (string-append  (join (cdr L)) " " (car L) )]))