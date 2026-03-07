;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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

;;; Custom handler for GDK events.

(define-module (schematic event handler)
  #:use-module (system foreign)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic hook)

  #:export (set-default-gdk-event-handler))


;;; The default event handler.  It does nothing more than running
;;; the user customizable hook gdk-event-hook() and return the
;;; event to GTK for further processing.
(define (default-event-handler *event *data)
  (run-hook gdk-event-hook *event *data)
  (gtk_main_do_event *event))

(define *default-event-handler
  (procedure->pointer void default-event-handler '(* *)))


;;; Function definitions:
;; void
;; gdk_event_handler_set (
;;   GdkEventFunc func,
;;   gpointer data,
;;   GDestroyNotify notify
;; )

;; void
;; (* GDestroyNotify) (
;;   gpointer data
;; )

;;; Example of GDestroyNotify function definition in Scheme:
;; (define (destroy-notify-func *data)
;;   (format (current-error-port) "(destroy-func ~A)\n" *data))
;;
;; (define *destroy-notify-func
;;   (procedure->pointer void destroy-notify-func '(*)))


;;; Sets the function to call to handle all events from GDK.
;;;
;;; Note that GTK+ uses this to install its own event handler, so it is
;;; usually not useful for GTK+ applications. (Although an application
;;; can call this function then call gtk_main_do_event() to pass events
;;; to GTK+.).
(define* (set-gdk-event-handler *handler
                                #:key
                                (*data %null-pointer)
                                (*destroy-notify-func %null-pointer))
  (gdk_event_handler_set *handler *data *destroy-notify-func))


(define (set-default-gdk-event-handler)
  "Set the default handler for GDK events that can be customized
using the hook GDK-EVENT-HOOK."
  (set-gdk-event-handler *default-event-handler))
