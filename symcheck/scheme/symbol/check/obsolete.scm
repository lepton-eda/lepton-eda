(define-module (symbol check obsolete)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-obsolete-attrib))

(define regex-old-pin (make-regexp "^pin[0-9]+$"))
(define regex-old-slot (make-regexp "^slot[0-9]+$"))

;;; Check symbol for old attributes.
(define (check-obsolete-attrib object)
  "Checks if OBJECT is an obsolete attribute."
  (and (attribute? object)
       (let ((name (attrib-name object))
             (value (attrib-value object)))
         (if (regexp-exec regex-old-pin name)
             (blame-object object
                           'error
                           (format #f
                                   (_ "Found old pin#=# attribute: ~A=~A\n")
                                   name
                                   value))
             (when (regexp-exec regex-old-slot name)
               (blame-object object
                             'error
                             (format #f
                                     (_ "Found old slot#=# attribute: ~A=~A\n")
                                     name
                                     value)))))))
