(use-modules (srfi srfi-1)
             (lepton version))

(test-begin "lepton-version")

;;; Full version list.
(define version (lepton-version))
;;; Elements.
(define prepend (lepton-version 'prepend))
(define dotted (lepton-version 'dotted))
(define date (lepton-version 'date))
(define git (lepton-version 'git))
(define git7 (lepton-version 'git7))
(define bugs (lepton-version 'bugs))
(define url (lepton-version 'url))
(define copyright (lepton-version 'copyright))
(define msg (lepton-version 'msg))

;;; Test the length of the returned list.
(test-eq 8 (length version))

;;; Every element of the list must be a string.
(test-assert (every string? version))

;;; Test all allowed symbols.
(test-assert (string? prepend))
(test-assert (string? dotted))
(test-assert (string? date))
(test-assert (string? git))
(test-assert (string? git7))
(test-assert (string? bugs))
(test-assert (string? url))
(test-assert (string? copyright))
(test-assert (string? msg))

;;; Test list element sequence.
(test-equal version
  (list prepend
        dotted
        date
        git
        bugs
        url
        copyright
        msg))

;;; Special case: git commit prefix.
(test-eq 7 (string-length git7))

(test-end "lepton-version")
