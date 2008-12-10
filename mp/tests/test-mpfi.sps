;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPFI numbers
;;;Date: Wed Dec 10, 2008
;;;Time-stamp: <2008-12-10 11:52:54 marco>
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
  (uriel lang)
  (uriel ffi)
  (uriel printing)
  (uriel test)
  (mp mpfr)
  (mp mpfi)
  (mp sizeof))

(check-set-mode! 'report-failed)



;;;; basic run

(define mpfi
  (make-caching-object-factory mpfi_init mpfi_clear
			       sizeof-mpfi_t 10))

(define (compensate-mpfi)
  (letrec ((p (compensate (mpfi)
		(with (mpfi p)))))
    p))

(define (mpfr->string o)
  (letrec
      ((str (compensate
		(malloc 1024)
	      (with
	       (primitive-free str))))
       (l (compensate-malloc/small)))
    (mpfr_get_str str l 10 0 o GMP_RNDN)
    (let* ((s (cstring->string str))
	   (x (let ((x (pointer-ref-c-signed-long l 0)))
		(if (char=? #\- (string-ref s 0))
		    (+ 1 x)
		  x)))
	   (i (substring s 0 x))
	   (f (substring s x (strlen str))))
      (print #f "~a.~a" i f))))

(with-compensations
  (letrec
      ((o (compensate-mpfi)))
    (mpfi_set_d o -123.456)
    (print #t "string rep: [~a, ~a]~%"
	   (mpfr->string (struct-mpfi-left-ref o))
	   (mpfr->string (struct-mpfi-right-ref o)))))



;;;; done

(mpfi 'purge)

(check-report)

;;; end of file
