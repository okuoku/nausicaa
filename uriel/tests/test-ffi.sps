;;;
;;;Part of: Uriel libraries
;;;Contents: tests for ffi library
;;;Date: Tue Nov 18, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; setup

(import (rnrs)
  (uriel printing)
  (uriel ffi)
  (uriel test))

(check-set-mode! 'report-failed)


;;;; code

(check
    (cstring->string (string->cstring "ciao"))
  => "ciao")

;; (check
;;     (let* ((f (make-c-callout signed-int (pointer)))
;; 	   (strlen (f (dlsym self 'strlen)))
;; 	   (g (make-c-callout pointer (signed-int)))
;; 	   (strerror (g (dlsym self 'strerror))))
;;       (cstring->string (strerror 0)))
;;   => "Success")

;; (check
;;     (let ((f (make-c-callout signed-int (pointer)))
;; 	  (g (make-c-callout signed-int (pointer))))
;;       (eq? f g))
;;   => #t)

;; (check
;;     (let ((f (make-c-callout signed-int (pointer)))
;; 	  (g (make-c-callout signed-int (signed-int))))
;;       (eq? f g))
;;   => #f)



;;;; done

(check-report)

;;; end of file
