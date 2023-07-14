;;; Test Scheme procedures related to pcb elements.

(use-modules (system foreign)
             (sch2pcb element))


(test-begin "pkg-line->element")

;;; Test that *PcbElement is created successfully from a proper
;;; string.
(let ((*element (pkg-line->element (string->pointer "PKG_DIP14(DIP14,U100,unknown)"))))
  (test-assert (not (null-pointer? *element)))
  (free-element *element))

(test-end)
