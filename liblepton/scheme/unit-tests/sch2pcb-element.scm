;;; Test Scheme procedures related to pcb elements.

(use-modules (system foreign)
             (sch2pcb element))


(test-begin "pkg-line->element")

;;; Test that *PcbElement is created successfully from a proper
;;; string.  Test its fields as well.
(let ((*element (pkg-line->element (string->pointer "PKG_DIP14(DIP14,U100,unknown)"))))
  (test-assert (not (null-pointer? *element)))
  (test-equal (pcb-element-description *element) "DIP14")
  (test-equal (pcb-element-refdes *element) "U100")
  (test-equal (pcb-element-value *element) "unknown")
  (free-element *element))

;;; Element's fields with whitespaces.
(let ((*element (pkg-line->element (string->pointer "PKG_DIP14(DIP14\t, U100,unknown  )"))))
  (test-assert (not (null-pointer? *element)))
  (test-equal (pcb-element-description *element) "DIP14_")
  (test-equal (pcb-element-refdes *element) "_U100")
  (test-equal (pcb-element-value *element) "unknown__")
  (free-element *element))

;;; Element with extra description arguments.
(let ((*element (pkg-line->element (string->pointer "PKG_100-Pin-jack(100-Pin-jack,refdes,value,Pin,jack)"))))
  (test-assert (not (null-pointer? *element)))
  (test-equal (pcb-element-description *element) "100-Pin-jack")
  (test-equal (pcb-element-refdes *element) "refdes")
  (test-equal (pcb-element-value *element) "value")
  (test-equal (pcb-element-pkg-name-fix *element) "Pin jack")
  (free-element *element))

;;; Element with value containing a comma.
(let ((*element (pkg-line->element (string->pointer "PKG_XXX(R0w8,R100,1k, 1%)"))))
  (test-assert (not (null-pointer? *element)))
  (test-equal (pcb-element-description *element) "R0w8")
  (test-equal (pcb-element-refdes *element) "R100")
  (test-equal (pcb-element-value *element) "1k,_1%")
  (free-element *element))

;;; Element with multi-word description and value containing a
;;; comma.
(let ((*element (pkg-line->element (string->pointer "PKG_YYY-multi-word(R0w8-multi-word,R100,1k, 1%,multi,word)"))))
  (test-assert (not (null-pointer? *element)))
  (test-equal (pcb-element-description *element) "R0w8-multi-word")
  (test-equal (pcb-element-refdes *element) "R100")
  (test-equal (pcb-element-value *element) "1k,_1%")
  (test-equal (pcb-element-pkg-name-fix *element) "multi word")
  (free-element *element))


;;; Wrong PKG_ strings.

;;; No left paren at all.
(test-assert (null-pointer? (pkg-line->element (string->pointer "PKG_DIP14"))))
;;; Missing "PKG_" prefix.
(test-assert (null-pointer? (pkg-line->element (string->pointer "DIP14(DIP14,U100,unknown)"))))
;;; Wrong amount of arguments in parens (less than 3).
(test-assert (null-pointer? (pkg-line->element (string->pointer "PKG_DIP14(DIP14,U100)"))))

(test-end)
