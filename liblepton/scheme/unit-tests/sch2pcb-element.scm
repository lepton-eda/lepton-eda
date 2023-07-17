;;; Test Scheme procedures related to pcb elements.

(use-modules (system foreign)
             (sch2pcb element))


(test-begin "pkg-line->element")

;;; Test that *PcbElement is created successfully from a proper
;;; string.
(let ((*element (pkg-line->element (string->pointer "PKG_DIP14(DIP14,U100,unknown)"))))
  (test-assert (not (null-pointer? *element)))
  (free-element *element))

;;; Wrong PKG_ strings.

;;; No left paren at all.
(test-assert (null-pointer? (pkg-line->element (string->pointer "PKG_DIP14"))))
;;; Missing "PKG_" prefix.
(test-assert (null-pointer? (pkg-line->element (string->pointer "DIP14(DIP14,U100,unknown)"))))
;;; Wrong amount of arguments in parens (less than 3).
(test-assert (null-pointer? (pkg-line->element (string->pointer "PKG_DIP14(DIP14,U100)"))))

(test-end)
