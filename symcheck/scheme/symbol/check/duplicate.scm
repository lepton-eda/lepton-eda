(define-module (symbol check duplicate)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (list->duplicate-list))

(define (list->duplicate-list ls f-less? f-equal?)
  "Sorts list LS using function F-LESS? for comparison and
transforms it into a list where duplicated members which values
are equal if compared using function F-EQUAL? are gathered
together into sublists."
  (fold-right
   (lambda (elem ret)
     (match ret
       (((x . xrest) . rest)
        (if (f-equal? elem x)
            `((,elem . (,x . ,xrest)) . ,rest)
            `(,elem . ,ret)))
       ((x . rest)
        (if (f-equal? elem x)
            `((,elem ,x) . ,rest)
            `(,elem . ,ret)))
       (_ `(,elem . ,ret))))
   '()
   (sort ls f-less?)))
