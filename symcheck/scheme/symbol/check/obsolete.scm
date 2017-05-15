(define-module (symbol check obsolete)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)
  #:use-module (symbol check attrib)
  #:use-module (symbol check pin-attrib)

  #:export (check-obsolete-pin-attrib
            check-obsolete-floating-attrib))

(define-syntax blame-error
  (syntax-rules ()
    ((_ object msg arg ...)
     (blame-object object 'error (format #f (gettext msg) arg ...)))))

(define regex-old-pin (make-regexp "^pin[0-9]+$"))
(define regex-old-slot (make-regexp "^slot[0-9]+$"))

;;; Blames OBJECT if it is an obsolete attribute. The attribute
;;; type is tested using PREDICATE. ERROR-MSG is used to blame it
;;; if its name matches REGEX. Returns OBJECT.
(define (check-obsolete-attrib object predicate regex error-msg)
  (when (predicate object)
    (let ((name (attrib-name object))
          (value (attrib-value object)))
      (when (regexp-exec regex name)
        (blame-error object error-msg name value))))
  object)

(define (check-obsolete-pin-attrib object)
  "Checks if OBJECT is an obsolete pin attribute."
  (check-obsolete-attrib object
                         pin-attrib?
                         regex-old-pin
                         "Obsolete pin#=# attribute: ~A=~A"))

(define (check-obsolete-floating-attrib object)
  "Checks if OBJECT is an obsolete floating attribute."
  (check-obsolete-attrib object
                         floating-attrib?
                         regex-old-slot
                         "Obsolete slot#=# attribute: ~A=~A"))
