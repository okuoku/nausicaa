;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test library for (records)
;;;Date: Sun Sep 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (records-lib)
  (export <alpha> <beta> <gamma>
	  <alpha>-with-record-fields-of
	  <beta>-with-record-fields-of
	  <gamma>-with-record-fields-of
	  make-<alpha>
	  make-<beta>
	  make-<gamma>)
  (import (rnrs)
    (classes))

  (define-class <alpha>
    (nongenerative records-lib:<alpha>)
    (fields (mutable a)
	    (immutable b)
	    (mutable c)))

  (define-class <beta>
    (nongenerative records-lib:<beta>)
    (parent <alpha>)
    (fields (mutable d)
	    (immutable e)
	    (mutable f))
    (virtual-fields (mutable def beta-def-ref beta-def-set!)))

  (define-class <gamma>
    (nongenerative records-lib:<gamma>)
    (parent <beta>)
    (fields (mutable g)
	    (immutable h)
	    (mutable i))
    (virtual-fields (mutable iota iota-ref iota-set!)
		    (mutable theta theta-ref theta-set!)))

  (define (beta-def-ref o)
    (with-fields ((o <beta>))
      (list o.d o.e o.f)))

  (define (beta-def-set! o ell)
    (with-fields ((o <beta>))
      (set! o.d (car ell))
      (set! o.f (cadr ell))))

  (define iota  91)
  (define theta 92)

  (define (iota-ref o)
    iota)

  (define (iota-set! o v)
    (set! iota v))

  (define (theta-ref o)
    theta)

  (define (theta-set! o v)
    (set! theta v))

  )

;;; end of file
