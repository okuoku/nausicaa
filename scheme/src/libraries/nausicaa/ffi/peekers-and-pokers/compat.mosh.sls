;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility peekers and pokers for Mosh
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa ffi peekers-and-pokers compat)
  (export
    ;;peekers
    pointer-ref-c-int8			pointer-ref-c-uint8
    pointer-ref-c-int16			pointer-ref-c-uint16
    pointer-ref-c-int32			pointer-ref-c-uint32
    pointer-ref-c-int64			pointer-ref-c-uint64
    pointer-ref-c-float			pointer-ref-c-double
    (rename (pointer-ref-c-pointer pointer-ref-c-void*))

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-int8!			pointer-set-c-uint8!
    pointer-set-c-int16!		pointer-set-c-uint16!
    pointer-set-c-int32!		pointer-set-c-uint32!
    pointer-set-c-int64!		pointer-set-c-uint64!
    pointer-set-c-float!		pointer-set-c-double!
    (rename (pointer-set-c-pointer! pointer-set-c-void*!))

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!
    pointer-set-c-pointer!)
  (import (rnrs)
    (only (mosh ffi)
	  pointer-ref-c-int8		pointer-ref-c-uint8
	  pointer-ref-c-int16		pointer-ref-c-uint16
	  pointer-ref-c-int32		pointer-ref-c-uint32
	  pointer-ref-c-int64		pointer-ref-c-uint64
	  pointer-ref-c-float		pointer-ref-c-double
	  pointer-ref-c-pointer

	  pointer-set-c-int8!		pointer-set-c-uint8!
	  pointer-set-c-int16!		pointer-set-c-uint16!
	  pointer-set-c-int32!		pointer-set-c-uint32!
	  pointer-set-c-int64!		pointer-set-c-uint64!
	  pointer-set-c-float!		pointer-set-c-double!
	  pointer-set-c-pointer!

	  pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
	  pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
	  pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
	  pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
	  pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
	  pointer-ref-c-pointer)
    (only (nausicaa ffi sizeof) c-sizeof))


;;;; pokers

(let-syntax ((define-signed-poker (syntax-rules ()
				    ((_ ?name ?type)
				     (define ?name (case (c-sizeof ?type)
						     ((1) pointer-set-c-int8!)
						     ((2) pointer-set-c-int16!)
						     ((4) pointer-set-c-int32!)
						     ((8) pointer-set-c-int64!)))))))
  (define-signed-poker pointer-set-c-signed-char!	char)
  (define-signed-poker pointer-set-c-signed-short!	short)
  (define-signed-poker pointer-set-c-signed-int!	int)
  (define-signed-poker pointer-set-c-signed-long!	long)
  (define-signed-poker pointer-set-c-signed-long-long!	long-long))

(let-syntax ((define-unsigned-poker (syntax-rules ()
				      ((_ ?name ?type)
				       (define ?name (case (c-sizeof ?type)
						       ((1) pointer-set-c-uint8!)
						       ((2) pointer-set-c-uint16!)
						       ((4) pointer-set-c-uint32!)
						       ((8) pointer-set-c-uint64!)))))))

  (define-unsigned-poker pointer-set-c-unsigned-char!		char)
  (define-unsigned-poker pointer-set-c-unsigned-short!		short)
  (define-unsigned-poker pointer-set-c-unsigned-int!		int)
  (define-unsigned-poker pointer-set-c-unsigned-long!		long)
  (define-unsigned-poker pointer-set-c-unsigned-long-long!	long-long))


;;;; done

)

;;; end of file
