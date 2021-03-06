;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test library for (records)
;;;Date: Sun Oct  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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

(library (records-lib-2)
  (export <beta*> <gamma*> <the-string*>)
  (import (rnrs)
    (records)
    (for (records-lib) expand run))

  (define (beta-def-ref o)
    (with-record-fields (((d e f) <beta> o))
      (list d e f)))

  (define (beta-def-set! o ell)
    (with-record-fields (((d f) <beta> o))
      (set! d (car ell))
      (set! f (cadr ell))))

  (define-record-extension <beta*>
    (parent <beta>)
    (fields (def beta-def-ref beta-def-set!)))

  ;;The following  definition is to  verify that DEFINE-RECORD-EXTENSION
  ;;is a  <definition> in a <body>,  so it allows  other <definition> to
  ;;appear after it.
  (define dummy 123)

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

  (define-record-extension <gamma*>
    (parent <gamma>)
    (fields (iota iota-ref iota-set!)
	    (theta theta-ref theta-set!)))

  (define-record-extension <the-string*>
    (parent <string>)
    (fields (length string-length #f)
	    (upcase string-upcase #f)
	    (dncase string-downcase #f)))

  )

;;; end of file
