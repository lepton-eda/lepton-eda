(define-module (symbol check line)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-line-size
            check-line))

(define (check-line-size object)
  "Checks if line OBJECT (line, net, or bus) has zero length."
  (let ((start (line-start object))
        (end (line-end object)))
    (when (equal? start end)
      (blame-object object
                    'error
                    (format #f
                            (_ "Zero length ~A at ~A")
                            (object-type object)
                            start)))))

(define (check-line object)
  "Checks line OBJECT:
  * Checks if it has zero length."
  (check-line-size object))
