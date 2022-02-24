;;; Lepton EDA Schematic Capture
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2013 gEDA Contributors
;;; Copyright (C) 2022 Lepton EDA Contributors
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

; This function resets all the refdes in a list of objects.
;
; objects: a list of gschem schematic objects

(use-modules (ice-9 regex)
             (lepton attrib))

(define (auto-refdes-reset! objects)

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
