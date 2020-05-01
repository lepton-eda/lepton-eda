(use-modules (netlist partlist)
             (netlist attrib compare))

(define (refdes>? a b)
  (not (or (equal? a b) (refdes<? a b))))

(define parts-with-refdes
  '(("X10" (refdes . "X10") (device . "unknown") (footprint . "DIP16"))
    ("X20" (refdes . "X20") (device . "unknown") (footprint . "DIP16"))
    ("X1" (refdes . "X1") (device . "DeViCe") (footprint . "DIP14"))
    ("X2" (refdes . "X2") (device . "DeViCe") (footprint . "DIP16"))
    ("X3" (refdes . "X3") (device . "DeViCe") (footprint . "DIP14"))
    ("X4" (refdes . "X4") (device . "DeViCe") (footprint . "DIP14"))
    ("N100" (refdes . "N100") (device . "Spice-nuLLor") (footprint . "NONE"))
    ("N101" (refdes . "N101") (device . "Spice-nuLLor") (footprint . "NONE"))
    ("N102" (refdes . "N102") (device . "Spice-nuLLor") (footprint . "NONE"))
    ("D1" (refdes . "D1") (device . "diodE") (footprint . "AXIAL2"))
    ("X7" (refdes . "X7") (device . "modeL") (footprint . "NONE"))
    ("N099" (refdes . "N099") (device . "Spice-nuLLor") (footprint . "NONE"))
    ("N99" (refdes . "N99") (device . "Spice-nuLLor") (footprint . "NONE"))
    ))

(define parts-without-refdes
  (map (lambda (a) (cons (car a) (cddr a))) parts-with-refdes))

;;; Unblock this if you want to test internal module functions
#|
(test-begin "partlist:pre-process-partlist" 3)

(define parts '(("X10" (refdes . "X10") (device . "DeViCe"))
                ("X1" (refdes . "X1") (device . "DeViCe"))
                ("X2" (refdes . "X2") (device . "DeViCe"))
                ("X3" (refdes . "X3") (device . "DeViCe"))
                ("X7" (refdes . "X7") (device . "modeL"))
                ("D1" (refdes . "D1") (device . "diode"))
                ("N100" (refdes . "N100") (device . "Spice-nuLLor"))))
(test-equal
    '(("X10" (refdes . "x10") (device . "DeViCe"))
      ("X1" (refdes . "x1") (device . "DeViCe"))
      ("X2" (refdes . "x2") (device . "DeViCe"))
      ("X3" (refdes . "x3") (device . "DeViCe"))
      ("X7" (refdes . "x7") (device . "model"))
      ("D1" (refdes . "d1") (device . "DIODE"))
      ("N100" (refdes . "n100") (device . "SPICE-nullor")))
  (pre-process-partlist parts #:letter-case '((refdes . lower) (device . smart))))

(test-equal
    '(("X10" (refdes . "x10") (device . "DEVICE"))
      ("X1" (refdes . "x1") (device . "DEVICE"))
      ("X2" (refdes . "x2") (device . "DEVICE"))
      ("X3" (refdes . "x3") (device . "DEVICE"))
      ("X7" (refdes . "x7") (device . "MODEL"))
      ("D1" (refdes . "d1") (device . "DIODE"))
      ("N100" (refdes . "n100") (device . "SPICE-NULLOR")))
  (pre-process-partlist parts #:letter-case '((refdes . lower) (device . upper))))

(test-equal
    '(("X7" (refdes . "X7") (device . "model"))
      ("N100" (refdes . "N100") (device . "spice-nullor")))
  (pre-process-partlist parts
                        #:letter-case '((refdes . upper) (device . lower))
                        #:remove '((device . "device") (refdes . "D1"))))

(test-end "partlist:pre-process-partlist")



(test-begin "partlist:reorder-part-attribs" 2)

(define part '("D1" (refdes . "D1") (device . "DIODE") (footprint . "unknown")))
(test-equal
    '("D1" (device . "DIODE") (refdes . "D1") (footprint . "unknown") (refdes . "D1"))
  (reorder-part-attribs part '(device refdes footprint refdes)))
(test-error #t (reorder-part-attribs part '(device value refdes)))
(test-end "partlist:reorder-part-attribs")

(test-begin "partlist:sort-partlist" 2)
(test-equal
    '(("D1" (refdes . "D1") (device . "diodE") (footprint . "AXIAL2"))
      ("N99" (refdes . "N99") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N100" (refdes . "N100") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N101" (refdes . "N101") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N102" (refdes . "N102") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N099" (refdes . "N099") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("X1" (refdes . "X1") (device . "DeViCe") (footprint . "DIP14"))
      ("X2" (refdes . "X2") (device . "DeViCe") (footprint . "DIP16"))
      ("X3" (refdes . "X3") (device . "DeViCe") (footprint . "DIP14"))
      ("X4" (refdes . "X4") (device . "DeViCe") (footprint . "DIP14"))
      ("X7" (refdes . "X7") (device . "modeL") (footprint . "NONE"))
      ("X10" (refdes . "X10") (device . "unknown") (footprint . "DIP16"))
      ("X20" (refdes . "X20") (device . "unknown") (footprint . "DIP16")))
  (sort-partlist parts-with-refdes))

(test-equal
    '(("X1" (device . "DeViCe") (footprint . "DIP14"))
      ("X3" (device . "DeViCe") (footprint . "DIP14"))
      ("X4" (device . "DeViCe") (footprint . "DIP14"))
      ("X2" (device . "DeViCe") (footprint . "DIP16"))
      ("N100" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N101" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N102" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N099" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N99" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("D1" (device . "diodE") (footprint . "AXIAL2"))
      ("X7" (device . "modeL") (footprint . "NONE"))
      ("X10" (device . "unknown") (footprint . "DIP16"))
      ("X20" (device . "unknown") (footprint . "DIP16")))
  (sort-partlist parts-without-refdes))

(test-end "partlist:sort-partlist")



(test-begin "partlist:group-partlist" 3)
(test-equal
    '((("X1" "X3" "X4") (device . "DeViCe") (footprint . "DIP14") (refdes "X1" "X3" "X4"))
      ("X2" (device . "DeViCe") (footprint . "DIP16") (refdes . "X2"))
      (("N99" "N100" "N101" "N102" "N099") (device . "Spice-nuLLor") (footprint . "NONE") (refdes  "N99" "N100" "N101" "N102" "N099"))
      ("D1" (device . "diodE") (footprint . "AXIAL2") (refdes . "D1"))
      ("X7" (device . "modeL") (footprint . "NONE") (refdes . "X7"))
      (("X10" "X20") (device . "unknown") (footprint . "DIP16") (refdes "X10" "X20")))
  (group-partlist parts-with-refdes 'refdes))

(test-equal
    '((("X1" "X3" "X4" "X2") (device . "DeViCe") (footprint "DIP14" "DIP14" "DIP14" "DIP16"))
      (("N100" "N101" "N102" "N099" "N99") (device . "Spice-nuLLor") (footprint "NONE" "NONE" "NONE" "NONE" "NONE"))
      ("D1" (device . "diodE") (footprint . "AXIAL2"))
      ("X7" (device . "modeL") (footprint . "NONE"))
      (("X10" "X20") (device . "unknown") (footprint "DIP16" "DIP16")))
  (group-partlist parts-without-refdes 'footprint))


(test-equal
    '(("D1" (footprint . "AXIAL2") (device . "diodE"))
      (("X1" "X3" "X4") (footprint . "DIP14") (device "DeViCe" "DeViCe" "DeViCe"))
      (("X2" "X10" "X20") (footprint . "DIP16") (device "DeViCe" "unknown" "unknown"))
      (("N100" "N101" "N102" "N099" "N99" "X7") (footprint . "NONE") (device "Spice-nuLLor" "Spice-nuLLor" "Spice-nuLLor" "Spice-nuLLor" "Spice-nuLLor" "modeL")))
  (group-partlist parts-without-refdes 'device))


(test-end "partlist:group-partlist")

(test-begin "partlist:process-partlist" 8)
(test-equal
    '(("D1" (refdes . "D1") (device . "diodE") (footprint . "AXIAL2"))
      ("N99" (refdes . "N99") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N100" (refdes . "N100") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N101" (refdes . "N101") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N102" (refdes . "N102") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N099" (refdes . "N099") (device . "Spice-nuLLor") (footprint . "NONE"))
      ("X1" (refdes . "X1") (device . "DeViCe") (footprint . "DIP14"))
      ("X2" (refdes . "X2") (device . "DeViCe") (footprint . "DIP16"))
      ("X3" (refdes . "X3") (device . "DeViCe") (footprint . "DIP14"))
      ("X4" (refdes . "X4") (device . "DeViCe") (footprint . "DIP14"))
      ("X7" (refdes . "X7") (device . "modeL") (footprint . "NONE"))
      ("X10" (refdes . "X10") (device . "unknown") (footprint . "DIP16"))
      ("X20" (refdes . "X20") (device . "unknown") (footprint . "DIP16")))
  (process-partlist parts-with-refdes))

(test-equal
    '(("X1" (device . "DeViCe") (footprint . "DIP14"))
      ("X3" (device . "DeViCe") (footprint . "DIP14"))
      ("X4" (device . "DeViCe") (footprint . "DIP14"))
      ("X2" (device . "DeViCe") (footprint . "DIP16"))
      ("N100" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N101" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N102" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N099" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("N99" (device . "Spice-nuLLor") (footprint . "NONE"))
      ("D1" (device . "diodE") (footprint . "AXIAL2"))
      ("X7" (device . "modeL") (footprint . "NONE"))
      ("X10" (device . "unknown") (footprint . "DIP16"))
      ("X20" (device . "unknown") (footprint . "DIP16")))
  (process-partlist parts-without-refdes))

(test-equal
    '(("D1" (refdes . "D1") (device . "diodE") (footprint . "AXIAL2"))
      (("N99" "N100" "N101" "N102" "N099") (refdes "N99" "N100" "N101" "N102" "N099") (device . "Spice-nuLLor") (footprint . "NONE"))
      (("X1" "X3" "X4") (refdes "X1" "X3" "X4") (device . "DeViCe") (footprint . "DIP14"))
      ("X2" (refdes . "X2") (device . "DeViCe") (footprint . "DIP16"))
      ("X7" (refdes . "X7") (device . "modeL") (footprint . "NONE"))
      (("X10" "X20") (refdes "X10" "X20") (device . "unknown") (footprint . "DIP16")))
  (process-partlist parts-with-refdes #:group-by 'refdes))

(test-equal
    '((("X1" "X3" "X4" "X2") (device . "DeViCe") (footprint "DIP14" "DIP14" "DIP14" "DIP16"))
      (("N100" "N101" "N102" "N099" "N99") (device . "Spice-nuLLor") (footprint "NONE" "NONE" "NONE" "NONE" "NONE"))
      ("D1" (device . "diodE") (footprint . "AXIAL2"))
      ("X7" (device . "modeL") (footprint . "NONE"))
      (("X10" "X20") (device . "unknown") (footprint "DIP16" "DIP16")))
  (process-partlist parts-without-refdes #:group-by 'footprint))

(test-equal
    '((("X1" "X3" "X4") (device "DeViCe" "DeViCe" "DeViCe") (footprint . "DIP14"))
      (("X2" "X10" "X20") (device "DeViCe" "unknown" "unknown") (footprint . "DIP16"))
      (("N100" "N101" "N102" "N099" "N99" "X7") (device "Spice-nuLLor" "Spice-nuLLor" "Spice-nuLLor" "Spice-nuLLor" "Spice-nuLLor" "modeL") (footprint . "NONE"))
      ("D1" (device . "diodE") (footprint . "AXIAL2")))
  (process-partlist parts-without-refdes #:group-by 'device))

(test-equal
    '(("D1" (footprint . "AXIAL2") (device . "diodE"))
      (("X1" "X3" "X4" "X2") (footprint "DIP14" "DIP14" "DIP14" "DIP16") (device . "DeViCe"))
      (("X10" "X20") (footprint "DIP16" "DIP16") (device . "unknown"))
      ("X7" (footprint . "NONE") (device . "modeL"))
      (("N100" "N101" "N102" "N099" "N99") (footprint "NONE" "NONE" "NONE" "NONE" "NONE") (device . "Spice-nuLLor")))
  (process-partlist parts-without-refdes
                    #:group-by 'footprint
                    #:sort-order '(footprint device)))

(test-equal
    '(("D1" (footprint . "AXIAL2") (refdes . "D1"))
      (("N99" "N100" "N101" "N102" "N099") (footprint . "NONE") (refdes "N99" "N100" "N101" "N102" "N099"))
      (("X1" "X3" "X4") (footprint . "DIP14") (refdes "X1" "X3" "X4"))
      ("X2" (footprint . "DIP16") (refdes . "X2"))
      ("X7" (footprint . "NONE") (refdes . "X7"))
      (("X10" "X20") (footprint . "DIP16") (refdes "X10" "X20")))
  (process-partlist parts-with-refdes
                    #:group-by 'refdes
                    #:sort-order '(refdes footprint)
                    #:output-order '(footprint refdes)))

(test-equal
    '((("X10" "X20") (refdes "X10" "X20") (footprint . "DIP16") (refdes "X10" "X20") (device . "unknown"))
      ("X7" (refdes . "X7") (footprint . "NONE") (refdes . "X7") (device . "modeL"))
      ("X2" (refdes . "X2") (footprint . "DIP16") (refdes . "X2") (device . "DeViCe"))
      (("X1" "X3" "X4") (refdes "X1" "X3" "X4") (footprint . "DIP14") (refdes "X1" "X3" "X4") (device . "DeViCe"))
      (("N99" "N100" "N101" "N102" "N099") (refdes "N99" "N100" "N101" "N102" "N099") (footprint . "NONE") (refdes "N99" "N100" "N101" "N102" "N099") (device . "Spice-nuLLor"))
      ("D1" (refdes . "D1") (footprint . "AXIAL2") (refdes . "D1") (device . "diodE")))
  (process-partlist parts-with-refdes
                    #:group-by 'refdes
                    #:sort-order `((refdes . ,refdes>?) (footprint . #f) (device . #f))
                    #:output-order '(refdes footprint refdes device)))

(test-end "partlist:process-partlist")

|#

(test-begin "partlist:partlist->string" 5)

(test-equal
  "X10, X20	DIP16	X10, X20	unknown
X7	NONE	X7	modeL
X2	DIP16	X2	DeViCe
X1, X3, X4	DIP14	X1, X3, X4	DeViCe
N099, N99-N102	NONE	N099, N99-N102	Spice-nuLLor
D1	AXIAL2	D1	diodE"
  (partlist->string
   parts-with-refdes
   #:group-by 'refdes
   #:reduce? #t
   #:sort-order `((refdes . ,refdes>?) (footprint . #f) (device . #f))
   #:output-order '(refdes footprint refdes device)))

;;; We have to escape backslashes in order to get what we want
(define document-header "\\documentclass[a4paper,11pt]{scrlettr}
\\usepackage[utf8x]{inputenc}
\\usepackage{lscape}
\\usepackage{longtable}

\\begin{document}
\\pagestyle{empty}
\\noindent\\begin{longtable}{p{.3\\linewidth}|p{.3\\linewidth}|p{.4\\linewidth}}
\\hline
")

(define transposed-document-header "\\documentclass[a4paper,11pt]{scrlettr}
\\usepackage[utf8x]{inputenc}
\\usepackage{lscape}
\\usepackage{longtable}

\\begin{document}
\\pagestyle{empty}
\\noindent\\begin{longtable}{}
\\hline
")

(define document-footer "
\\end{longtable}

\\end{document}
")

(test-equal
    (string-append document-header
                   "diodE & AXIAL2 & D1 \\\\
Spice-nuLLor & NONE & N099; N99--N102 \\\\
DeViCe & DIP14 & X1; X3; X4 \\\\
DeViCe & DIP16 & X2 \\\\
modeL & NONE & X7 \\\\
unknown & DIP16 & X10; X20"
                   document-footer)
  (partlist->string
   parts-with-refdes
   #:group-by 'refdes
   #:sort-order '(refdes footprint device)
   #:output-order '(device footprint refdes)
   #:header document-header
   #:footer document-footer
   #:prepend-names? #f
   #:reduce? #t
   #:row-separator " \\\\\n"
   #:column-separator " & "
   #:group-separator "; "
   #:subgroup-separator "--"
   #:transpose? #f))

(test-equal
    (string-append transposed-document-header
                   "device & diodE & Spice-nuLLor & DeViCe & DeViCe & modeL & unknown \\\\
footprint & AXIAL2 & NONE & DIP14 & DIP16 & NONE & DIP16 \\\\
refdes & D1 & N099; N99--N102 & X1; X3; X4 & X2 & X7 & X10; X20"
                   document-footer)
  (partlist->string
   parts-with-refdes
   #:group-by 'refdes
   #:sort-order '(refdes footprint device)
   #:output-order '(device footprint refdes)
   #:header transposed-document-header
   #:footer document-footer
   #:prepend-names? #t
   #:reduce? #t
   #:row-separator " \\\\\n"
   #:column-separator " & "
   #:group-separator "; "
   #:subgroup-separator "--"
   #:transpose? #t))

(test-equal
    (string-append document-header
                   "diodE & AXIAL2 & D1 & 1 \\\\
Spice-nuLLor & NONE & N099; N99--N102 & 5 \\\\
DeViCe & DIP14 & X1; X3; X4 & 3 \\\\
DeViCe & DIP16 & X2 & 1 \\\\
modeL & NONE & X7 & 1 \\\\
unknown & DIP16 & X10; X20 & 2"
                   document-footer)
  (partlist->string
   parts-with-refdes
   #:group-by 'refdes
   #:sort-order '(refdes footprint device)
   #:output-order '(device footprint refdes #{}#)
   #:header document-header
   #:footer document-footer
   #:prepend-names? #f
   #:reduce? #t
   #:row-separator " \\\\\n"
   #:column-separator " & "
   #:group-separator "; "
   #:subgroup-separator "--"
   #:transpose? #f))

(test-equal
    (string-append document-header
                   "Spice-nuLLor & NONE & N099; N99--N102 & 5 \\\\
DeViCe & DIP14 & X1; X3; X4 & 3 \\\\
unknown & DIP16 & X10; X20 & 2 \\\\
diodE & AXIAL2 & D1 & 1 \\\\
DeViCe & DIP16 & X2 & 1 \\\\
modeL & NONE & X7 & 1"
                   document-footer)
  (partlist->string
   parts-with-refdes
   #:group-by 'refdes
   #:sort-order `((#{}# . ,>) (refdes . ,refdes<?) (device . ,string<?) (footprint . ,string<?))
   #:output-order '(device footprint refdes #{}#)
   #:header document-header
   #:footer document-footer
   #:prepend-names? #f
   #:reduce? #t
   #:row-separator " \\\\\n"
   #:column-separator " & "
   #:group-separator "; "
   #:subgroup-separator "--"
   #:transpose? #f
   ))

(test-equal
  "d1___DIODE___AXIAL2
n099, n99-n102___SPICE-nullor___NONE
x1, x3, x4___DeViCe___DIP14
x2___DeViCe___DIP16
x7___model___NONE
x10, x20___unknown___DIP16"
  (partlist->string
   parts-with-refdes
   #:group-by 'refdes
   #:header ""
   #:footer ""
   #:reduce? #t
   #:column-separator "___"
   #:letter-case '((refdes . lower) (device . smart))
   ))

(test-equal
  "N099, N99, N100, N101, N102___spice-nullor___NONE
X7___model___NONE
X10, X20___unknown___DIP16"
  (partlist->string
   parts-with-refdes
   #:group-by 'refdes
   #:header ""
   #:footer ""
   #:reduce? #f
   #:column-separator "___"
   #:letter-case '((refdes . upper) (device . lower))
   #:remove '((device . "device") (refdes . "D1"))
   ))

(test-error
  (partlist->string
   parts-with-refdes
   #:output-order '(device value refdes)))

(test-equal
  "DeViCe, DeViCe, DeViCe___DIP14
DeViCe, unknown, unknown___DIP16
Spice-nuLLor, Spice-nuLLor, Spice-nuLLor, Spice-nuLLor, Spice-nuLLor, modeL___NONE
diodE___AXIAL2"
  (partlist->string
   parts-without-refdes
   #:group-by 'device
   #:header ""
   #:footer ""
   #:reduce? #t
   #:column-separator "___"
   ))

(test-equal
  "diodE___AXIAL2___1
DeViCe___DIP14, DIP14, DIP14, DIP16___4
unknown___DIP16, DIP16___2
modeL___NONE___1
Spice-nuLLor___NONE, NONE, NONE, NONE, NONE___5"
  (partlist->string
   parts-without-refdes
   #:group-by 'footprint
   #:sort-order '(footprint device)
   #:output-order '(device footprint #{}#)
   #:header ""
   #:footer ""
   #:column-separator "___"
   ))

(test-end "partlist:partlist->string")
