(define-module (symbol check text)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-text-visibility))

(define (check-text-visibility object)
  "Checks if text OBJECT being non-attribute has inappropriate
visibility mode."
  (and (text? object)
       (not (attribute? object))
       (not (eq? (text-attribute-mode object) 'both))
       (blame-object object
                     'warning
                     (format #f
                             (_ "Found a simple text object with only SHOW_NAME or SHOW_VALUE set [~A]\n")
                             (text-string object)))))
