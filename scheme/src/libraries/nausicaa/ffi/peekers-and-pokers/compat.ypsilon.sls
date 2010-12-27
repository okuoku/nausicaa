;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility peekers and pokers for Ypsilon
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
(library (ffi peekers-and-pokers compat)
  (export
    ;;peekers
    pointer-ref-c-int8			pointer-ref-c-uint8
    pointer-ref-c-int16			pointer-ref-c-uint16
    pointer-ref-c-int32			pointer-ref-c-uint32
    pointer-ref-c-int64			pointer-ref-c-uint64
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-void*

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
    pointer-set-c-void*!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!
    pointer-set-c-pointer!)
  (import (rnrs)
    (only (ypsilon ffi)
	  make-bytevector-mapping
	  bytevector-c-void*-ref
	  bytevector-c-void*-set!)
    (only (ffi pointers) integer->pointer pointer->integer)
    (only (ffi sizeof) c-sizeof))


;;;; peekers

;;We have to make the peekers work even when the position is negative.
;;
;;The  procedure MAKE-BYTEVECTOR-MAPPING accepts  2 arguments:  an exact
;;integer  representing  the memory  address,  the  number  of bytes  to
;;consider part of the bytevector.  We precompute the address+offset and
;;use the  resulting address as first  byte of the  bytevector; then the
;;size of the bytevector is just the size of the data we have to access.

(let-syntax ((define-peeker (syntax-rules ()
			      ((_ ?name ?getter ?sizeof-data)
			       (define (?name pointer position)
				 (?getter
				  (make-bytevector-mapping (+ position (pointer->integer pointer))
							   ?sizeof-data)
				  0)))
			      ((_ ?name ?getter ?sizeof-data ?mapper)
			       (define (?name pointer position)
				 (?mapper
				  (?getter
				   (make-bytevector-mapping (+ position (pointer->integer pointer))
							    ?sizeof-data)
				   0)))))))

  (define-peeker pointer-ref-c-int8	bytevector-s8-ref         1)
  (define-peeker pointer-ref-c-int16	bytevector-s16-native-ref 2)
  (define-peeker pointer-ref-c-int32	bytevector-s32-native-ref 4)
  (define-peeker pointer-ref-c-int64	bytevector-s64-native-ref 8)

  (define-peeker pointer-ref-c-uint8	bytevector-u8-ref         1)
  (define-peeker pointer-ref-c-uint16	bytevector-u16-native-ref 2)
  (define-peeker pointer-ref-c-uint32	bytevector-u32-native-ref 4)
  (define-peeker pointer-ref-c-uint64	bytevector-u64-native-ref 8)

  (define-peeker pointer-ref-c-float	bytevector-ieee-single-native-ref (c-sizeof float))
  (define-peeker pointer-ref-c-double	bytevector-ieee-double-native-ref (c-sizeof double))
  (define-peeker pointer-ref-c-void*	bytevector-c-void*-ref (c-sizeof pointer) integer->pointer))

(let-syntax ((define-signed-peeker (syntax-rules ()
				     ((_ ?name ?sizeof-data)
				      (define ?name (case ?sizeof-data
						      ((1) pointer-ref-c-int8)
						      ((2) pointer-ref-c-int16)
						      ((4) pointer-ref-c-int32)
						      ((8) pointer-ref-c-int64)))))))

  (define-signed-peeker pointer-ref-c-signed-char	(c-sizeof char))
  (define-signed-peeker pointer-ref-c-signed-short	(c-sizeof short))
  (define-signed-peeker pointer-ref-c-signed-int	(c-sizeof int))
  (define-signed-peeker pointer-ref-c-signed-long	(c-sizeof long))
  (define-signed-peeker pointer-ref-c-signed-long-long	(c-sizeof long-long)))

(let-syntax ((define-unsigned-peeker (syntax-rules ()
				       ((_ ?name ?sizeof-data)
					(define ?name (case ?sizeof-data
							((1) pointer-ref-c-uint8)
							((2) pointer-ref-c-uint16)
							((4) pointer-ref-c-uint32)
							((8) pointer-ref-c-uint64)))))))
  (define-unsigned-peeker pointer-ref-c-unsigned-char	(c-sizeof char))
  (define-unsigned-peeker pointer-ref-c-unsigned-short	(c-sizeof short))
  (define-unsigned-peeker pointer-ref-c-unsigned-int	(c-sizeof int))
  (define-unsigned-peeker pointer-ref-c-unsigned-long	(c-sizeof long))
  (define-unsigned-peeker pointer-ref-c-unsigned-long-long (c-sizeof long-long)))

(define pointer-ref-c-pointer pointer-ref-c-void*)


;;;; pokers

;;We have  to make the pokers  work even when the  position is negative.
;;The  procedure MAKE-BYTEVECTOR-MAPPING accepts  2 arguments:  an exact
;;integer  representing  the memory  address,  the  number  of bytes  to
;;consider part of the bytevector.  We precompute the address+offset and
;;use the  resulting address as first  byte of the  bytevector; then the
;;size of the bytevector is just the size of the data we have to access.

(let-syntax ((define-poker (syntax-rules ()
			     ((_ ?name ?setter ?sizeof-data)
			      (define (?name pointer position value)
				(?setter
				 (make-bytevector-mapping (+ position (pointer->integer pointer))
							  ?sizeof-data)
				 0 value)))
			     ((_ ?name ?setter ?sizeof-data ?mapper)
			      (define (?name pointer position value)
				(?setter
				 (make-bytevector-mapping (+ position (pointer->integer pointer))
							  ?sizeof-data)
				 0 (?mapper value)))))))

  (define-poker pointer-set-c-int8!	bytevector-s8-set!         1)
  (define-poker pointer-set-c-int16!	bytevector-s16-native-set! 2)
  (define-poker pointer-set-c-int32!	bytevector-s32-native-set! 4)
  (define-poker pointer-set-c-int64!	bytevector-s64-native-set! 8)

  (define-poker pointer-set-c-uint8!	bytevector-u8-set!         1)
  (define-poker pointer-set-c-uint16!	bytevector-u16-native-set! 2)
  (define-poker pointer-set-c-uint32!	bytevector-u32-native-set! 4)
  (define-poker pointer-set-c-uint64!	bytevector-u64-native-set! 8)

  (define-poker pointer-set-c-float!	bytevector-ieee-single-native-set! (c-sizeof float))
  (define-poker pointer-set-c-double!	bytevector-ieee-double-native-set! (c-sizeof double))
  (define-poker pointer-set-c-void*!	bytevector-c-void*-set! (c-sizeof pointer pointer->integer)))

(let-syntax ((define-signed-poker (syntax-rules ()
				    ((_ ?name ?sizeof-data)
				     (define ?name (case ?sizeof-data
						     ((1) pointer-set-c-int8!)
						     ((2) pointer-set-c-int16!)
						     ((4) pointer-set-c-int32!)
						     ((8) pointer-set-c-int64!)))))))
  (define-signed-poker pointer-set-c-signed-char!		(c-sizeof char))
  (define-signed-poker pointer-set-c-signed-short!		(c-sizeof short))
  (define-signed-poker pointer-set-c-signed-int!		(c-sizeof int))
  (define-signed-poker pointer-set-c-signed-long!		(c-sizeof long))
  (define-signed-poker pointer-set-c-signed-long-long!		(c-sizeof long-long)))

(let-syntax ((define-unsigned-poker (syntax-rules ()
				      ((_ ?name ?sizeof-data)
				       (define ?name (case ?sizeof-data
						       ((1) pointer-set-c-uint8!)
						       ((2) pointer-set-c-uint16!)
						       ((4) pointer-set-c-uint32!)
						       ((8) pointer-set-c-uint64!)))))))
  (define-unsigned-poker pointer-set-c-unsigned-char!		(c-sizeof char))
  (define-unsigned-poker pointer-set-c-unsigned-short!		(c-sizeof short))
  (define-unsigned-poker pointer-set-c-unsigned-int!		(c-sizeof int))
  (define-unsigned-poker pointer-set-c-unsigned-long!		(c-sizeof long))
  (define-unsigned-poker pointer-set-c-unsigned-long-long!	(c-sizeof long-long)))

(define pointer-set-c-pointer! pointer-set-c-void*!)


;;;; done

)

;;; end of file
