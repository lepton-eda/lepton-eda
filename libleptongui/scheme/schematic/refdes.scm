;;; Lepton EDA Schematic Capture
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2014 gEDA Contributors
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (schematic refdes)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)

  #:use-module (lepton attrib)
  #:use-module (lepton page)

  #:export (auto-refdes-reset!
            auto-uref-set-page-offset
            auto-uref))

(define (auto-refdes-reset! objects)
  "Resets all the refdeses in OBJECTS which should be a list of
schematic objects."

    (let (
        (question-mark "?")
        (refdes-name "refdes")
        (refdes-regex "^([^0-9])+([0-9]+)(.*)$")
        (refdes-regex-prefix 1)
        (refdes-regex-suffix 3))


    ; This function rebuilds the refdes string with the question-mark in place
    ; of the number.
    ;
    ; If this function can't parse the refdes, it returns the original refdes
    ; unmodified. This unparsable case includes when the refdes number is
    ; already a question mark.
    ;
    ; refdes: a string containing the refdes
    ; return: a string containing the reset refdes

    (define (rebuild-refdes refdes)
        (let ((match (string-match refdes-regex refdes)))

        (if match
            (string-append (match:substring match refdes-regex-prefix)
                           question-mark
                           (match:substring match refdes-regex-suffix))
            refdes)))


    ; This predicate determines if an object represets a refdes and can be
    ; reset.
    ;
    ; object: a gschem schematic object
    ; return: #t if the object represents a refdes attribute that can be reset

    (define (resetable-refdes? object)
        (and
            (attribute? object)
            (not (attrib-inherited? object))
            (string=? refdes-name (attrib-name object))))


    ; This function resets the refdes of the given object
    ;
    ; object: a gschem schematic object representing a refdes attribute

    (define (reset-refdes! object)
        (let ((next-refdes (rebuild-refdes (attrib-value object))))

        (set-attrib-value! object next-refdes)))


    ; iterate through the list of objects and reset all the refdes attributes

    (for-each reset-refdes! (filter resetable-refdes? objects))))


(define auto-uref-page-offset 0)

;; Redefine value of page-offset.
;; Refdeses will be numbered from integer multiples of page-offset,
;; depending on the lowest refdes value found on the page.
;; If lowest value is 323 and page offset is 100, then next refdes
;; will be 301.
;; Setting to 0 disables the feature.
(define (auto-uref-set-page-offset new-offset)
  (set! auto-uref-page-offset new-offset))

;; Modify attributes of an object to assign next unused refdes value
(define (auto-uref attribs)

  ; Return (prefix . number) on match or #f on failure
  (define (split-value value)
    (let ((match (string-match "^([A-Za-z]*)([0-9]+)$" value)))
      (if match
          (cons (match:substring match 1)
                (string->number (match:substring match 2)))
          #f)))

  ; Extract prefix from a refdes attribute value
  (define (get-prefix value)
    (let ((prefix (string-match "^[A-Za-z]*" value)))
      (if prefix (match:substring prefix) #f)))

  ; Filter non-inherited refdes values
  (define (refdes-attrs attribs)
    (filter (lambda (a)
              (and
                (not (attrib-inherited? a))
                (string=? "refdes" (attrib-name a))))
            attribs))

  ; Extract numbers from refdeses that have given prefix
  (define (extract-numbers object prefix)
    (let* ((refdeses (refdes-attrs (object-attribs object)))
           (vals (map attrib-value refdeses))
           (prefix-numbers (filter-map split-value vals))
           (numbers (filter-map (lambda (n.v)
                                  (if (string=? prefix (car n.v))
                                      (cdr n.v)
                                      #f))
                                prefix-numbers)))
      numbers))

  ; Collect all numbers associated with prefix on current page
  (define (collect-all-numbers prefix)
    (let ((objects (page-contents (active-page))))
      (concatenate (map (lambda (o)
                          (extract-numbers o prefix))
                        objects))))

  ; Return first number not present in used greater or equal to minimum
  (define (find-first-unused used minimum)
    (define (go n xs)
      (cond ((null? xs) n)
            ((< n (car xs)) n)
            ((= n (car xs)) (go (1+ n) (cdr xs)))
            (else (go n (cdr xs)))))
    (go minimum used))

  ; Do the work - first check if attributes contain refdes with prefix
  (let* ((refdeses (refdes-attrs attribs))
         (refdes (if (null? refdeses)
                     #f
                     (car refdeses)))
         (prefix (if refdes
                     (get-prefix (attrib-value refdes))
                     #f)))
    (if prefix
        (let* ((used-nums (sort-list (collect-all-numbers prefix) <))
               ; minimum value considering the page-offset
               (min-num (if (or
                              (null? used-nums)
                              (<= auto-uref-page-offset 0))
                            0
                            (* (floor (/ (car used-nums)
                                         auto-uref-page-offset))
                               auto-uref-page-offset)))
               ; next refdes number to be assigned
               (next-num (find-first-unused used-nums (1+ min-num))))
          ;(simple-format #t "~A: ~A -> ~A~%"
          ;                  prefix
          ;                  used-nums
          ;                  next-num)
          (set-attrib-value!
            refdes
            (string-append prefix (number->string next-num)))))))
