;;; Lepton EDA Schematic Capture
;;; Copyright (C) 2013-2014 Patrick Bernaud <patrickb@chez.com>
;;; Copyright (C) 2020 Lepton EDA Contributors
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

(use-modules (gschem hook)
             (lepton object)
             (geda attrib))

(define (place-netname-attribute attribute)
  ;; Only attached netname attributes
  (and=> (attrib-attachment attribute)
         (lambda (object)
           ;; with auto-place-attribs.scm
           (set-default-position object attribute 
                                 (get-net-connection-sides object)
                                 default-position-of-text-attributes))))

(define (place-netname-attribute-handler objects)
  (for-each (lambda (o)
              (and (attribute? o)
                   (string=? (attrib-name o) "netname")
                   (place-netname-attribute o)))
            objects))
