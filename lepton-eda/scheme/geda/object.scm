;; Lepton EDA library - Scheme API
;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;; Copyright (C) 2012-2016 gEDA Contributors
;; Copyright (C) 2017-2020 Lepton EDA Contributors
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;

(define-module (geda object)

  #:use-module (geda deprecated)
  #:use-module (lepton object)

  #:re-export (object-type
               object-id
               object?
               object-type?
               copy-object
               object-color
               set-object-color!
               object-connections
               object-component
               ;; Lines
               line?
               set-line!
               make-line
               line-info
               line-start
               line-end
               ;; Nets
               net?
               make-net
               ;; Buses
               bus?
               make-bus
               ;; Pins
               pin?
               net-pin?
               bus-pin?
               make-net-pin
               make-bus-pin
               ;; Boxes
               box?
               set-box!
               make-box
               box-info
               box-top-left
               box-bottom-right
               ;; Circles
               circle?
               set-circle!
               make-circle
               circle-info
               circle-center
               circle-radius
               ;; Arcs
               arc?
               set-arc!
               make-arc
               arc-info
               arc-center
               arc-radius
               arc-start-angle
               arc-sweep-angle
               arc-end-angle
               ;; Paths
               path?
               make-path
               path-length
               path-ref
               path-remove!
               path-insert!
               ;; Pictures
               picture?
               set-picture!
               make-picture/vector
               picture-info
               picture-filename
               picture-top-left
               picture-bottom-right
               picture-angle
               picture-mirror?
               ;; Text
               text?
               set-text!
               make-text
               text-info
               text-anchor
               text-align
               text-angle
               text-string
               set-text-string!
               text-size
               text-visible?
               set-text-visibility!
               text-attribute-mode
               ;; Component objects
               component?
               set-component!
               set-component-with-transform!
               make-component
               make-component/library
               component-info
               component-basename
               component-filename
               component-position
               component-angle
               component-mirror?
               component-locked?
               component-contents
               component-append!
               component-remove!
               ;; Fill and stroke
               object-stroke
               set-object-stroke!
               object-stroke-width
               object-stroke-cap
               object-stroke-dash
               object-fill
               set-object-fill!
               ;; Object bounds
               object-bounds
               fold-bounds
               ;; Object transformations
               translate-objects!
               rotate-objects!
               mirror-objects!
               object-selectable?
               set-object-selectable!
               object-embedded?
               set-object-embedded!))

(deprecated-module-log-warning! "(lepton object)")
