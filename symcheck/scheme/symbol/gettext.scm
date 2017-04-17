(define-module (symbol gettext)

  #:export (_ N_))

(define %symcheck-gettext-domain "lepton-symcheck")

(define (_ msg) (gettext msg %symcheck-gettext-domain))

(define (N_ msgid msgid-plural n)
  (ngettext msgid msgid-plural n %symcheck-gettext-domain))
