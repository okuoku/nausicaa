;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: cleanup functions
;;;Date: Mon Nov 24, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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



(library (cleanup)
  (export
    cleanup
    register-cleanup-function
    forget-cleanup-function)
  (import (rnrs))

  (define cleanup-thunks '())

  (define (register-cleanup-function thunk)
    (set! cleanup-thunks (cons thunk cleanup-thunks)))

  (define (forget-cleanup-function thunk)
    (set! cleanup-thunks (remove thunk cleanup-thunks)))

  (define (cleanup)
    (for-each
	(lambda (thunk)
	  (thunk))
      cleanup-thunks)))

;;; end of file
