#lang scheme

;Element counter
(define (lenght lis)(
                        if(equal? lis '())
                          0
                          (+ 1 (lenght (cdr lis)))
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
                                if (< index (lenght lis)) 
                                   (getElement lis index)
                                   "Index out of bound."
                               )
 )
