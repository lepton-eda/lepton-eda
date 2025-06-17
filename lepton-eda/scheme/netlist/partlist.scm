;;; Lepton EDA netlister
;;; Copyright (C) 2016 gEDA Contributors
;;; Copyright (C) 2018 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


;;; This module provides an API to export partlists.

;;; Partlist is a list of parts each of which is a pair of the
;;; form (package . attrib-alist) where attrib-alist is an alist
;;; of part attributes.


(define-module (netlist partlist)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (netlist partlist common)
  #:use-module (netlist attrib compare)
  #:export (partlist->string))


;;; Modifies VALUE so the part before the first "-" is uppercase
;;; and the part after it is lowercase
(define (string-mixcase value)
  (let ((pos (1+ (string-index value #\-))))
    (string-append (string-upcase (string-take value pos))
                   (string-downcase (string-drop value pos)))))


;;; Alist of functions for smart choosing of attribute case
(define device-values
  `( ;; device= attribute values to upcase
    (capacitor . ,string-upcase)
    (current_source . ,string-upcase)
    (diode . ,string-upcase)
    (ic . ,string-upcase)
    (inductor . ,string-upcase)
    (mesfet_transistor . ,string-upcase)
    (nfet_transistor . ,string-upcase)
    (nmos_transistor . ,string-upcase)
    (npn_transistor . ,string-upcase)
    (pfet_transistor . ,string-upcase)
    (pmos_transistor . ,string-upcase)
    (pnp_transistor . ,string-upcase)
    (polarized_capacitor . ,string-upcase)
    (resistor . ,string-upcase)
    (voltage_source . ,string-upcase)

    ;; device= attribute values to upcase the first part and
    ;; downcase the second
    (spice-ccvs . ,string-mixcase)
    (spice-cccs . ,string-mixcase)
    (spice-vcvs . ,string-mixcase)
    (spice-vccs . ,string-mixcase)
    (spice-nullor . ,string-mixcase)

    ;; device= attribute values to downcase
    (model . ,string-downcase)
    (include . ,string-downcase)
    (options . ,string-downcase)
    (directive . ,string-downcase)
    ))


;;; This symbol converted to string will give an empty string so
;;; it is safe to use it
(define quantity-symbol '#{}#)

;;; Transposes a matrix, that is a list of lists
(define (transpose ls)
  "Transposes list of lists LS."
  (apply map list ls))

;;; Returns a list of attribute names of PART
(define (part->attrib-names part)
  (match part
    ((package . ((names . values) ...))
     names)))

;;; Transforms PART-LIST into a package list
(define (part-list->package-list part-list)
  (define (part->package part)
    (match part
      ((package . ((names . values) ...))
       `(,package ,@values))))
  (if (null? part-list)
      '()
      `((package . ,(part->attrib-names (car part-list)))
        ,@(map part->package part-list))))

;;; Transforms PACKAGE-LIST into a part list
(define (package-list->part-list package-list)
  (define (make-part attrib-names attrib-values)
    (match (map cons attrib-names attrib-values)
      ((('package . package-id) . attribs)
       `(,package-id . ,attribs))))
  (if (null? package-list)
      '()
      (match package-list
        ((attrib-names . packs)
         (map (cut make-part attrib-names <>) packs)))))

;;; Changes case of some attribs in the PART-LIST according to
;;; ATTRIB-CASE-LIST where the latter is an alist containing pairs
;;; of attribute name and any symbol from the list
;;; '(upper lower smart).
(define (modify-partlist-case attrib-case-list part-list)
  (define (string-smartcase value)
    (let ((func (or (assq-ref device-values
                              (string->symbol (string-downcase value)))
                    identity)))
      (func value)))

  (define (case-func name)
    (case (assq-ref attrib-case-list name)
       ((upper) string-upcase)
       ((lower) string-downcase)
       ((smart) (if (eq? name 'device)
                    string-smartcase
                    identity))
       (else identity)))

  (define (change-case attrib-list)
    (match attrib-list
      ((name . values)
       (let ((func (case-func name)))
         `(,name . ,(map (cut func <>) values))))))

  (package-list->part-list
   (transpose
    (map (cut change-case <>)
         (transpose (part-list->package-list part-list))))))

;;; Filters PART-LIST removing parts which name-value pairs are
;;; in FILTER-ALIST.
(define (filter-attrib-value part-list filter-alist)
  ;; Returns #f if one of attributes of PART is in FILTER-ALIST,
  ;; otherwise returns PART.
  (define (filter-part filter-alist part)
    (match part
      ((package . attribs)
       ;; We loop through filter-alist here because if we would
       ;; loop through attribs, we cannot use assq-ref since
       ;; filter-alist may contain several attributes with the
       ;; same name
       (let loop ((ls filter-alist))
         (match ls
           (() part)
           (((name . value) . rest)
            (and (not (and=> (assq-ref attribs name)
                             (lambda (x) (string=? x value))))
                 (loop rest))))))))

  (filter-map (cut filter-part filter-alist <>) part-list))


(define %default-attrib-sort-list
  `((refdes . ,refdes<?)
    (value . ,value<?)
    (device . ,string<?)
    (footprint . ,string<?)
    (,quantity-symbol . ,<)))

;;; Compares two lists ALS and BLS member by member using
;;; functions FUNC=? and FUNC<? to compare its members. ALS is
;;; considered to be less than BLS if either its first member is
;;; less than (FUNC<?) the first member of BLS, or they are equal
;;; (FUNC=?) and the process is repeated for the next members. ALS
;;; is also considered to be less than BLS if all its members are
;;; the same than those of BLS, but it is shorter.
(define (list<? func=? func<? a b)
  (let loop ((als (if (list? a) a (list a)))
             (bls (if (list? b) b (list b))))
    (and (not (null? bls))
         (or (null? als)
             (func<? (car als) (car bls))
             (and (func=? (car als) (car bls))
                  (loop (cdr als) (cdr bls)))))))

;;; Compares attributes of two parts A and B using for comparison
;;; functions from the list LESS-LS and returns #t if A is less
;;; than B, otherwise returns #f. If a member of LESS-LS is #f
;;; then corresponding members of A and B are not compared. The
;;; function is used to sort list attribute lists.
(define (part<? less-ls a b)
  (and (not (null? less-ls))
       (let ((less-func (car less-ls)))
         (if less-func
             (let ((a-value (cdar a))
                   (b-value (cdar b)))
               (or (if (or (list? a-value) (list? b-value))
                       (list<? string=? less-func a-value b-value)
                       (less-func a-value b-value))
                   (and (equal? a-value b-value)
                        (part<? (cdr less-ls) (cdr a) (cdr b)))))
             (part<? (cdr less-ls) (cdr a) (cdr b))))))

;;; Preliminary processes PART-LIST and applies user case changes
;;; specified by LETTER-CASE and attribute filtering specified by
;;; REMOVE.  LETTER-CASE is an alist of attributes of the form
;;;   '((name . value) ...)
;;; where value may be one of 'upper, 'lower, or 'smart.
;;; REMOVE is an alist of attributes of the same form defining which
;;; parts to remove from PART-LIST.
(define* (pre-process-partlist part-list
                               #:key
                               (letter-case '())
                               (remove '()))
  (filter-attrib-value
   (modify-partlist-case letter-case part-list)
   remove))

;;; Sorts PART-LIST. Optional argument SORT-BY may be used to
;;; change the sorting order. If SORT-BY is not given, an internal
;;; sort function is used that sorts parts using default attrib sort
;;; list; if it is an alist of the form
;;;   `((attrib-name1 . ,proc1) (attrib-name2 . ,proc2) ...)
;;; then attributes will be sorted in the given order using
;;; corresponding procedures from the alist; if it is just a list of
;;; attribute names, the attributes will be sorted in the given order
;;; using corresponding default procedures predefined for each
;;; attribute name, if there is no such default procedure defined, the
;;; case sensitive string sort procedure string<? is used.
;;;
;;; So the following calls are appropriate:
;;;   (sort-partlist parts
;;;      #:sort-by `((refdes ,string-ci<?) (footprint ,string>?)))
;;;
;;;   (sort-partlist parts #:sort-by '(refdes device value))
(define* (sort-partlist part-list
                        #:key
                        (sort-by %default-attrib-sort-list))
  (define (get-default-func attrib)
    (or (assq-ref %default-attrib-sort-list attrib)
        string<?))

  (define (names->funcs names less-list)
    (map (cut assq-ref less-list <>) names))

  (define (form-func-list sort-by example-part)
    ;; Head is the package part for which a sorting function is not defined
    (cons #f
          (match sort-by
            (((name . value) ...)
             (names->funcs (part->attrib-names example-part) sort-by))
            (_ (map get-default-func sort-by)))))

  (if (list? sort-by)
      (let ((func-list (form-func-list sort-by (car part-list))))
        (sort part-list
              (lambda (a b) (part<? func-list a b))))
      (error "Inappropriate #:sort-by type" sort-by)))

;;; Transforms PART-LIST into a part list grouped by given
;;; ATTRIBUTE which is an attribute name, that is a sorted part list
;;; where parts with the same attributes excluding ATTRIBUTE are
;;; merged into a \"grouped\" part, that is a pair of the form
;;;   (list-of-packages . attrib-alist)
;;; where the value of ATTRIBUTE in attrib-alist is the list of
;;; corresponding ATTRIBUTE values of the merged parts.
(define (group-partlist part-list attribute)
  ;; Move the column which number is ID to the end of LS
  (define (reorder-by-index id ls)
    (if (or (null? ls)
            (null? (cdr ls)))
        ls
        (call-with-values (cut split-at ls id)
          (lambda (a b) `(,@a ,@(cdr b) ,(car b))))))

  (define (reorder-partlist-by-index ls attrib id)
    (match (transpose ls)
      ((package-list . attrib-list)
       (transpose `(,package-list . ,(reorder-by-index id attrib-list))))))

  (define (group-parts ls)
    (define (join a b)
      (if (list? b) `(,a . ,b) `(,a ,b)))

    (define (group-attribs a b rest)
      (reverse
       (match a
         ((afirst . arest)
          `((,afirst . ,(join arest b)) . ,rest)))))

    (fold-right
     (lambda (next prev)
       (if (null? prev)
           `(,next)
           (let* ((part-a next)
                  (part-b (car prev))
                  (package-a (car part-a))
                  (package-b (car part-b))
                  (attribs-a (cdr part-a))
                  (attribs-b (cdr part-b))
                  (other-parts (cdr prev)))
             (match `(,(reverse attribs-a) . ,(reverse attribs-b))
               ;; rest attribs are equal
               (((a . rest) . (b . rest))
                `((,(join package-a package-b)
                   . ,(group-attribs a (cdr b) rest))
                  . ,other-parts))
               (_ `(,next . ,prev))))))
     '()
     ls))

  (define (get-index name parts)
    (list-index (cut eq? name <>)
                (part->attrib-names (car parts))))

  (if attribute
      (group-parts
       (sort-partlist
        (reorder-partlist-by-index part-list
                                   attribute
                                   (get-index attribute part-list))))
      part-list))

;;; Reorders attributes in PART by given ORDER which must be a
;;; list of attribute names specifying their new order in output.
(define (reorder-part-attribs part order)
  (define (reorder-all attribs order quantity)
    (map (lambda (name)
           (cons name
                 (or (and (eq? name quantity-symbol)
                          quantity)
                     (assq-ref attribs name)
                     (error "Invalid attrib name in output order list"
                            name))))
         order))

  (match part
    ((package . attribs)
     `(,package
       . ,(reorder-all attribs
                       order
                       (if (list? package) (length package) 1))))
    (_ (error "Invalid part format" part))))

;;; Adds quantity to every part in grouped PART-LIST where members
;;; may be lists of parts.
(define (add-quantity part-list)
  (define (package-length package)
    (if (list? package) (length package) 1))
  (map (lambda (part)
         (match part
           ((package . attribs)
            `(,package
              . ((,quantity-symbol . ,(package-length package))
                 . ,attribs)))))
       part-list))

;;; Reorders PART-LIST accordingly to given ORDER.
(define (reorder-partlist part-list order)
  (map (cut reorder-part-attribs <> order) part-list))

;;; Groups PART-LIST by attribute GROUP-BY and then sorts the list
;;; according to SORT-ORDER.  SORT-ORDER may be an alist consisting of
;;; attribute names with corresponding sorting functions, or just a
;;; list of attribute names in which case the parts are sorted using
;;; default functions defined for corresponding attributes. If no
;;; sorting function is defined for an attribute name, string<? is
;;; used. No sorting is carried out for attribute names missing in the
;;; list. The priority of sorting lowers from the first element to the
;;; last, that is, last attributes are taken into account only if all
;;; preceding attributes are equal. OUTPUT-ORDER is a list of
;;; attribute names specifying the order in which the sorted part
;;; attributes are output.
(define* (process-partlist part-list
                           #:key
                           (group-by #f)
                           (sort-order #f)
                           (output-order #f))

  (define (sort-order->reorder-order order example-part)
    (match order
      (#f (part->attrib-names example-part))
      (((name . value) ...) name)
      (_ order)))

  (if (null? part-list)
      '()
      (let ((sorted-ls (sort-partlist
                        (reorder-partlist
                         (add-quantity
                          (group-partlist part-list group-by))
                         (sort-order->reorder-order
                          sort-order
                          (car part-list)))
                        #:sort-by
                        (or sort-order
                            %default-attrib-sort-list))))
        (if output-order
            (reorder-partlist sorted-ls output-order)
            sorted-ls))))


;;; Transform strings in LS into pairs of the form
;;; (string . number) and sort the resulting list using refdes<?.
(define (alpha-numeric-sort ls)
  ;; Break up string S and produce a pair of the form
  ;; (string . number) where the number is a suffix of S
  ;; consisting of digits.
  (define (break-up s)
    (let* ((skip-pos (1+ (string-skip-right s char-numeric?)))
           (s-numeric-part (string-drop s skip-pos))
           (pos (and (not (string=? "" s-numeric-part))
                     (+ skip-pos (string-skip s-numeric-part
                                              (lambda (x)
                                                (eq? x #\0)))))))
      (if pos
          `(,(string-take s pos)
            . ,(string->number (string-drop s pos)))
          `(,s . #f))))

  (map (cut break-up <>) (sort ls refdes<?)))

;;; Given a pair of a string and a list of numbers reduce the
;;; list in order it to contain pairs of (start . end) instead of
;;; sets of sequential ordered numbers
(define (reduce-groups ls)
  ;; If previous number is less than next by one then increase it,
  ;; otherwise prepend new number
  (define (join-numbers a b)
    (match b
      ;; First element of b is a pair of start and end numbers
      (((start . end) . rest)
       (if (eq? a (1- start))
           `((,a . ,end) . ,rest)
           `(,a . ,b)))
      ;; First element of b is a number or #f
      ((num . rest)
       (if (and a num (eq? a (1- num)))
           `((,a . ,num) . ,rest)
           `(,a . ,b)))))

  ;; Merge elements having the same string part into one element of
  ;; the form (string . list-of-numbers)
  (define (merge next prev)
    (match `(,next . ,prev)
      (((s . n1) . ((s . n2) . rest))
       `((,s . ,(join-numbers n1 n2))  . ,rest))
      (((s . n) . rest)
       `((,s ,n)  . ,rest))))

  (fold-right merge '() ls))


(define* (partlist->string part-list
                           #:key
                           (group-by #f)
                           (sort-order #f)
                           (output-order #f)
                           (header "")
                           (footer "")
                           (prepend-names? #f)
                           (quantity-header "quantity")
                           (reduce? #f)
                           (row-separator "\n")
                           (column-separator "\t")
                           (group-separator ", ")
                           (subgroup-separator "-")
                           (transpose? #f)
                           (letter-case '())
                           (remove '()))
  "Takes PART-LIST  and outputs a string representing a
table of attribute values of the list. The following optional
arguments may be used:

  GROUP-BY is a name of the attribute by which the part list is
  grouped.

  SORT-ORDER may be an alist consisting of attribute names with
  corresponding sorting functions, or just a list of attribute names
  in which case the parts are sorted using default functions defined
  for corresponding attributes. If no sorting function is defined
  for an attribute name, the case sensitive string sorting function
  string<? is used. No sorting is carried out for attribute names
  missing in the list. The priority of sorting lowers from the first
  element to the last, that is, last attributes are taken into
  account only if all previous attributes are equal.

  OUTPUT-ORDER is a list of attribute names specifying the order in
  which the part attributes are output.

  HEADER is a text which is output without any change before
  the attribute table contents.

  FOOTER is a text which is output without any change after the
  attribute table contents.

  PREPEND-NAMES? is predicate specifying if the first line
  of the table must be the list of output attribute names.

  QUANTITY-HEADER is a text which is output for quantity column if
  PREPEND-NAMES? is not #f.

  REDUCE? is a predicate specifying if lists of grouped values must
  be reduced so that attribute values in series contain only their
  first and last values, e.g. the list of strings \"R1\", \"R2\",
  and \"R3\" can be transformed to \"R1-R3\".

  ROW-SEPARATOR is a separator string for table rows. Default
  value is \"\\n\" (newline).

  COLUMN-SEPARATOR is a separator string for table
  columns. Default value is \"\\t\" (tabulation).

  GROUP-SEPARATOR is a separator string for group members. Default
  value is \", \".

  SUBGROUP-SEPARATOR is like GROUP-SEPARATOR, but it is used
  between first and last elements of each subgroup (if any) of
  reduced groups if REDUCE? is not #f. Default value is \"-\".

  TRANSPOSE? is a predicate used to specify that the resulting
  table must be transposed, that is, part attributes are output in
  columns rather than rows.

  LETTER-CASE is an attribute value case transformation alist of
  the form
    '((NAME . VALUE) ...)
  where NAME is an attribute name and VALUE may be one of 'upper,
  'lower, or 'smart. The first two values mean that letters are to
  be transformed to upper or lower case, accordingly. The last one
  is used for special case transformations where an appropriate
  transformation function is seached for by attribute name in an
  internal function table.

  REMOVE is an alist of attributes of the form
    '((NAME . VALUE) ...)
  where NAME is a symbol and VALUE is a string, and both define
  the attribute which, being found on a part, tells that all
  attributes of the part are to be removed from the output. Such
  filtering is carried out after letter case modification."


  ;; Transforms part list containing part groups into list of strings.
  (define (group-list->string-list ls)
    (define (group->string entry)
      (match entry
        ((base . (rest ...))
         (map
          (lambda (x)
            (match x
              ((start . end)
               (string-append
                base
                (number->string start)
                (if (= 1 (- end start))
                    group-separator
                    subgroup-separator)
                base
                (number->string end)))
              (#f base)
              (_ (string-append base (number->string x)))))
          rest))
        ((base . #f)
         `(,base))
        ((base . rest)
         (list (string-append base (number->string rest))))))

    (append-map group->string ls))

  (define (format-group ls)
    (string-join
     (group-list->string-list
      ((if reduce? reduce-groups identity) (alpha-numeric-sort ls)))
     group-separator))

  (define (format-column attrib)
    (match attrib
      ((name . value)
       (match value
         ;; groups
         ((? list?) (format-group value))
         ;; quantity
         ((? number?) (number->string value))
         ;; others
         (_ value)))
      (_ #f)))

  (define (format-row part)
    (string-join (map format-column part)
                 column-separator))

  (define (format-body ls)
    (string-join
     (map format-row (if transpose? (transpose ls) ls))
     row-separator))

  (define (throw-packages ls)
    (map cdr ls))

  (define (prepend-names ls)
    (define (name->pair name)
      `(,name . ,(if (eq? name quantity-symbol)
                     quantity-header
                     (symbol->string name))))
    (if (null? ls)
        '()
        (cons (map name->pair (map car (car ls))) ls)))

  (let ((parts (throw-packages
                (process-partlist
                 (pre-process-partlist part-list
                                       #:letter-case letter-case
                                       #:remove remove)
                 #:group-by group-by
                 #:sort-order sort-order
                 #:output-order output-order))))
    (format #f "~A~A~A"
            header
            (format-body (if prepend-names?
                             (prepend-names parts)
                             parts))
            footer)))
