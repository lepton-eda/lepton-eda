(define-module (symbol check alignment)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-pin-alignment))

(define %grid-step 100)

(define (check-pin-alignment pin)
  "Checks if both PIN ends of given object are aligned by grid.
Returns PIN."
  (define x car)
  (define y cdr)
  (define (on-grid? num)
    (= 0 (euclidean-remainder num %grid-step)))

  (define (is-on-grid? coord)
    (and (on-grid? (x coord))
         (on-grid? (y coord))))

  (define (check-on-grid pin msg-type msg coord)
    ;; Consider line-start to be the first pin point
    (let ((num (if (eq? msg-type 'error) 1 2)))
      (or (is-on-grid? coord)
          (blame-object pin
                        msg-type
                        (format #f
                                msg
                                num
                                (x coord)
                                num
                                (y coord))))))

  ;; line-start is the connectible point (whichend)
  (check-on-grid pin
                 'error
                 "Connectible end of pin is off grid (x~A=~A,y~A=~A)"
                 (line-start pin))
  (check-on-grid pin
                 'warning
                 "Non-connectible end of pin is off grid (x~A=~A,y~A=~A)"
                 (line-end pin)))
