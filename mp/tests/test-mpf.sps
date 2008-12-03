;;;
;;;Part of: Nausicaa/MPFR
;;;Contents: tests for the MPF numbers
;;;Date: Thu Nov 27, 2008
;;;Time-stamp: <2008-11-29 13:44:42 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



;;;; setup

(import (rnrs)
  (uriel lang)
  (uriel ffi)
  (uriel printing)
  (uriel test)
  (mp mpf)
  (mp sizeof))

(check-set-mode! 'report-failed)



;;;; basic run

(define mpf
  (make-caching-object-factory mpf_init mpf_clear
			       sizeof-mpf 10))

(define (compensate-mpf)
  (letrec ((p (compensate (mpf)
		(with (mpf p)))))
    p))

(define (mpf->string o)
  (letrec
      ((str (compensate
		(malloc 1024)
	      (with
	       (primitive-free str))))
       (l (compensate-malloc/small)))
    (mpf_get_str str l 10 0 o)
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
      ((o (compensate-mpf)))
    (mpf_set_d o -123.456)
    (print #t "string rep: ~a~%" (mpf->string o))))



;;;; done

(mpf 'purge)

(check-report)

;;; end of file
