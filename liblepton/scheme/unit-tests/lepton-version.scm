(use-modules (srfi srfi-1)
             (lepton version))

(test-begin "lepton-version")

;;; Full version list.
(define version-data (lepton-version-data))
;;; Elements.
(define prepend (lepton-version-ref 'prepend))
(define dotted (lepton-version-ref 'dotted))
(define date (lepton-version-ref 'date))
(define git (lepton-version-ref 'git))
(define git7 (lepton-version-ref 'git7))
(define bugs (lepton-version-ref 'bugs))
(define url (lepton-version-ref 'url))
(define copyright (lepton-version-ref 'copyright))

;;; Test the length of the returned list.
(test-eq 7 (length version-data))

;;; Every element of the list must be a string.
(test-assert (every string? version-data))

;;; Test all allowed symbols.
(test-assert (string? prepend))
(test-assert (string? dotted))
(test-assert (string? date))
(test-assert (string? git))
(test-assert (string? git7))
(test-assert (string? bugs))
(test-assert (string? url))
(test-assert (string? copyright))

;;; Test list element sequence.
(test-equal version-data
  (list prepend
        dotted
        date
        git
        bugs
        url
        copyright))

;;; Special case: git commit prefix.
(test-eq 7 (string-length git7))

(test-assert (string? (lepton-version)))

(test-equal (lepton-version "~A ~A ~A ~A ~A ~A ~A"
                            'prepend
                            'dotted
                            'date
                            'git
                            'bugs
                            'url
                            'copyright)
  (string-join (lepton-version-data) " " 'infix))

(test-end "lepton-version")
