(define-module (symbol check arc)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-arc-radius
            check-arc-angle
            check-arc))

(define (check-arc-radius object)
"Checks that arc OBJECT has non-zero radius."
  (and (= 0 (arc-radius object))
       (blame-object object
                     'error
                     (format #f
                             (_ "Zero radius arc at ~A")
                             (arc-center object)))))

(define (check-arc-angle object)
  "Checks that arc OBJECT has non-zero angle."
  (and (= 0 (euclidean-remainder
             (- (arc-end-angle object)
                (arc-start-angle object))
             360))
       (blame-object object
                     'error
                     (format #f
                             (_ "Zero angle arc at ~A")
                             (arc-center object)))))

(define (check-arc object)
  "Checks arc OBJECT:
  * Checks that it has non-zero radius.
  * Checks that it has non-zero angle."
  (check-arc-radius object)
  (check-arc-angle object))
