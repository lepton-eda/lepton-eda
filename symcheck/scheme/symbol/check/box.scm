(define-module (symbol check box)
  #:use-module (ice-9 match)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-box-size
            check-box))

(define (check-box-size object)
  "Checks box OBJECT size."
  (define (blame-zero-box object)
    (blame-object object
                     'error
                     (format #f
                             (_ "Zero sized box at ~A")
                             (box-top-left object))))

  (match `(,(box-top-left object) . ,(box-bottom-right object))
    (((x . y0) . (x . y1)) (blame-zero-box object))
    (((x0 . y) . (x1 . y)) (blame-zero-box object))
    (_ #f)))


(define (check-box object)
  "Checks box OBJECT:
  * Checks that it has non-zero size."
  (check-box-size object))
