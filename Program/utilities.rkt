#lang scheme
(provide length)
(provide getElement)
(provide getIndex)
(provide getPos)

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