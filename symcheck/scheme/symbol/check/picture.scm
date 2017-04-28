(define-module (symbol check picture)
  #:use-module (ice-9 match)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-picture-size
            check-picture-file
            check-picture))

(define (check-picture-size object)
  "Checks picture OBJECT size."
  (define (blame-zero-picture object)
    (blame-object object
                     'error
                     (format #f
                             (_ "Zero sized picture at ~A")
                             (picture-top-left object))))

  (match `(,(picture-top-left object) . ,(picture-bottom-right object))
    (((x . y0) . (x . y1)) (blame-zero-picture object))
    (((x0 . y) . (x1 . y)) (blame-zero-picture object))
    (_ #f)))


(define (check-picture-file object)
  "Checks that picture OBJECT's file exists and is readable."
  (let ((filename (picture-filename object)))
   (and (not (access? filename R_OK))
        (blame-object object
                      'error
                      (format #f
                              (_ "Picture file ~S does not exist or is not readable.")
                              filename)))))

(define (check-picture object)
  "Checks picture OBJECT:
  * Checks if its file exist and is readable.
  * Checks that it has non-zero size on canvas."
  (check-picture-file object)
  (check-picture-size object))
