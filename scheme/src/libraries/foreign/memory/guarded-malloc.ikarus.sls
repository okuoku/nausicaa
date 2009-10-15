;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: guarded memory allocation with Ikarus Scheme
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


(library (foreign memory guarded-malloc)
  (export malloc/guarded calloc/guarded)
  (import (rnrs)
    (only (foreign memory alloc)
	  malloc calloc primitive-free)
    (only (cleanup-handlers)
	  register-cleanup-function)
    (only (ikarus)
	  make-guardian))

  (define block-guardian (make-guardian))

  (define (block-cleanup)
    (do ((p (block-guardian) (block-guardian)))
	((not p))
      (p)))

  (define (malloc/guarded size)
    (let ((p (malloc size)))
      (block-guardian (lambda () (primitive-free p)))
      p))

  (define (calloc/guarded count element-size)
    (let ((p (calloc count element-size)))
      (block-guardian (lambda () (primitive-free p)))
      p))

  (register-cleanup-function block-cleanup))

;;; end of file
