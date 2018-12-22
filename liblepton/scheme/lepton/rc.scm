;;; Lepton EDA library - Scheme API
;;; Copyright (C) 1998-2017 gEDA Contributors
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02111-1301 USA.

;;; Lepton rc files processing procedures.

(define-module (lepton rc)
  #:use-module (ice-9 match)
  #:use-module (geda attrib)
  #:use-module (geda core gettext)
  #:use-module (geda object)
  #:use-module (geda log)

  #:export (always-promote-attributes
            eligible-attribute?
            promotable-attribute?
            promote-invisible
            promote-invisible-attribs?))

;;; Controls if invisible attribs are promoted.
(define %promote-invisible? #f)

(define (set-promote-invisible! allow?)
  (set! %promote-invisible? allow?)
  %promote-invisible?)

(define (promote-invisible allow?)
  "Checks ALLOW? and determines if invisible attribs should be
promoted. Returns #t if ALLOW? is equal to \"enabled\" or #t,
otherwise returns #f."
  (set-promote-invisible!
   (match allow?
     ((or "enabled" #t) #t)
     (_ #f))))

(define (promote-invisible-attribs?)
  "Returns #t if promotion of invisible attribs is enabled,
otherwise returns #f."
  %promote-invisible?)

;;; List of attributes to always promote.
(define %attribs-to-promote '("symversion"))

;;; Redefine the list of attribs to promote. Returns the new list.
(define (set-attribs-to-promote! ls)
  ;; Always promote "symversion" attribute, even if it is invisible.
  (set! %attribs-to-promote (delete-duplicates (append ls "symversion")))
  %attribs-to-promote)

;;; Checks the list of ATTRIBS that will be promoted on symbol
;;; insertion. Returns #f if ATTRIBS is neither a space separated
;;; string (deprecated), nor a list of strings. Otherwise returns
;;; the list of the attribs given.
(define (check-attribs-to-promote attribs)
  (define delimiters
    (string->char-set " \n\t"))

  (match attribs
    (((? string? x) ...)
     x)
    (_
     (log! 'warning
           (_ "WARNING: 'always-promote-attributes' must be a list of strings."))
     #f)))

(define (always-promote-attributes attribs)
  "Checks and sets the list of ATTRIBS to promote for new symbols
in schematic. Returns the list if ATTRIBS is a valid list,
otherwise returns #f."
  (and=> (check-attribs-to-promote attribs) set-attribs-to-promote!))

(define (promotable-attribute? attrib)
  "Returns #t if ATTRIB is promotable, otherwise returns #f."
  (not (not (member attrib %attribs-to-promote))))

(define (eligible-attribute? object)
  "Returns #t if OBJECT is eligible attribute for promotion,
otherwise returns #f."
  (and (attribute? object)
       (promotable-attribute? (attrib-name object))
       ;; Return #f if object is invisible and we do not want to
       ;; promote invisible text.
       (or (text-visible? object)
           (promote-invisible-attribs?))))
