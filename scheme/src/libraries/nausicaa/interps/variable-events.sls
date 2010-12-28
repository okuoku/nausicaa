;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: interface to the variable event tuple
;;;Date: Tue Dec 14, 2010
;;;
;;;Abstract
;;;
;;;	When  a  persistent variable  is  referenced  or  mutated in  an
;;;	interp, a  "variable event" is  generated; it is described  by a
;;;	record.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (nausicaa interps variable-events)
  (export
    <variable-event> <variable-reference> <variable-mutation>
    kont:
    name:
    value:)
  (import (nausicaa)
    (nausicaa language makers))


(define-auxiliary-syntaxes
  kont:
  name:
  mutated:
  value:)

(define-class <variable-event>
  (nongenerative interps:<variable-event>)
  (fields (immutable kont)))

(define-class <variable-reference>
  (nongenerative interps:<variable-reference>)
  (inherit <variable-event>)
  (fields (immutable name))
  (maker ()
	 (kont:	#f	(mandatory))
	 (name:	#f	(mandatory))))

(define-class <variable-mutation>
  (nongenerative interps:<variable-mutation>)
  (inherit <variable-event>)
  (fields (immutable name)
	  (immutable value))
  (maker ()
	 (kont:		#f	(mandatory))
	 (name:		#f	(mandatory))
	 (value:	#f	(mandatory))))


;;;; done

)

;;; end of file
