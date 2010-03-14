;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Expat
;;;Contents: compensated constructors
;;;Date: Tue Dec  1, 2009
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


(library (foreign xml expat compensated)
  (export
    xml-parser-create/c
    )
  (import (rnrs)
    (only (language-extensions)
	  begin0-let)
    (compensations)
    (only (foreign ffi pointers) pointer-null pointer-null?)
    (only (foreign memory conditions) raise-out-of-memory)
    (foreign xml expat))


(define (xml-parser-create/c encoding)
  (letrec ((parser (compensate
		       (begin0-let ((p (xml-parser-create pointer-null)))
			 (when (pointer-null? p)
			   (raise-out-of-memory 'xml-parser-create #f)))
		     (with
		      (xml-parser-free parser)))))
    parser))


;;;; done

)

;;; end of file
