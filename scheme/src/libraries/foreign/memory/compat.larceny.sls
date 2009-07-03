;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility memory functions for Larceny
;;;Date: Fri Dec 26, 2008
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



;;;; setup

(library (foreign memory compat)
  (export

    platform-free			platform-malloc
    platform-calloc			platform-realloc

    memset				memmove
    memcpy				memcmp

    ;;pointers
    pointer?
    integer->pointer			pointer->integer
    pointer-null			pointer-null?
    pointer-diff			pointer-add
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

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
    (primitives
     foreign-procedure
     %peek8 %peek8u %peek16 %peek16u %peek32 %peek32u %peek-pointer
     %poke8 %poke8u %poke16 %poke16u %poke32 %poke32u %poke-pointer
     void*-double-set! void*-double-ref void*-float-set! void*-float-ref
     void*? void*-rt record-constructor void*->address)
    (foreign ffi sizeof))


;;;; pointers

;;NULL pointers are #f for Larceny.   But then VOID*? does not accept #f
;;as valid.

(define (integer->pointer value)
  (if (= 0 value)
      #f
    (begin
      (unless (integer? value)
	(assertion-violation 'integer->pointer
	  "expected integer value" value))
      ((record-constructor void*-rt) value))))

(define (pointer->integer pointer)
  (if pointer
      (begin
	(unless (pointer? pointer)
	  (assertion-violation 'pointer->integer
	    "expected pointer value" pointer))
	(void*->address pointer))
    0))

(define (pointer? obj)
  (if obj (void*? obj) #t))

(define pointer-null
  #f)

(define (pointer-null? pointer)
  (if pointer #f #t))

(define (pointer-diff pointer-1 pointer-2)
  (- (pointer->integer pointer-1)
     (pointer->integer pointer-2)))

(define (pointer-add pointer offset)
  (integer->pointer (+ (pointer->integer pointer)
		       offset)))

(define-syntax define-pointer-comparison
  (syntax-rules ()
    ((_ ?name ?func)
     (define ?name
       (case-lambda
	(()
	 #f)
	((pointer)
	 #t)
	((pointer-a pointer-b)
	 (?func (pointer->integer pointer-a)
		(pointer->integer pointer-b)))
	((pointer-a pointer-b . pointers)
	 (apply ?func (map pointer->integer
			(cons pointer-a (cons pointer-b pointers))))))))))

(define-pointer-comparison pointer=? =)
(define-pointer-comparison pointer<? <)
(define-pointer-comparison pointer>? >)
(define-pointer-comparison pointer<=? <=)
(define-pointer-comparison pointer>=? >=)

(define pointer<>?
  (case-lambda
   (()
    #f)
   ((pointer . pointers)
    (not (apply pointer=? pointer pointers)))))


;;;; foreign functions

(define platform-free
  (foreign-procedure "free" '(void*) 'void))

(define platform-malloc
  (foreign-procedure "malloc" '(unsigned) 'void*))

(define platform-realloc
  (foreign-procedure "realloc" '(void* unsigned) 'void*))

(define platform-calloc
  (foreign-procedure "calloc" '(unsigned unsigned) 'void*))

(define memset
  (foreign-procedure "memset" '(void* int unsigned) 'void*))

(define memmove
  (foreign-procedure "memmove" '(void* void* unsigned) 'void*))

(define memcpy
  (foreign-procedure "memcpy" '(void* void* unsigned) 'void*))

(define memcmp
  (foreign-procedure "memcmp" '(void* void* unsigned) 'int))



;;;; peekers

;;Larceny does  not provide peekers for  64 bits values, so  we do this.
;;They are slow, but we rarely  need 64 bits, no?  Anyway, wadda ya want
;;from me?!?
(define (%peek64 pointer)
  (do ((i 0 (+ 1 i))
       (p pointer (+ 1 p))
       (bv (make-bytevector 8)))
      ((= i 8)
       (bytevector-s64-native-ref bv 0))
    (bytevector-u8-set! bv i (%peek8u p))))

(define (%peek64u pointer)
  (do ((i 0 (+ 1 i))
       (p pointer (+ 1 p))
       (bv (make-bytevector 8)))
      ((= i 8)
       (bytevector-u64-native-ref bv 0))
    (bytevector-u8-set! bv i (%peek8u p))))

(define-syntax define-peeker
  (syntax-rules ()
    ((_ ?name ?peeker)
     (define (?name pointer position)
       (?peeker (+ position (pointer->integer pointer)))))))

(define-syntax define-signed-peeker
  (syntax-rules ()
    ((_ ?name ?sizeof-data)
     (define ?name (case ?sizeof-data
		     ((1) pointer-ref-c-int8)
		     ((2) pointer-ref-c-int16)
		     ((4) pointer-ref-c-int32)
		     ((8) pointer-ref-c-int64))))))

(define-syntax define-unsigned-peeker
  (syntax-rules ()
    ((_ ?name ?sizeof-data)
     (define ?name (case ?sizeof-data
		     ((1) pointer-ref-c-uint8)
		     ((2) pointer-ref-c-uint16)
		     ((4) pointer-ref-c-uint32)
		     ((8) pointer-ref-c-uint64))))))

(define-peeker pointer-ref-c-int8	%peek8)
(define-peeker pointer-ref-c-int16	%peek16)
(define-peeker pointer-ref-c-int32	%peek32)
(define-peeker pointer-ref-c-int64	%peek64)

(define-peeker pointer-ref-c-uint8	%peek8u)
(define-peeker pointer-ref-c-uint16	%peek16u)
(define-peeker pointer-ref-c-uint32	%peek32u)
(define-peeker pointer-ref-c-uint64	%peek64u)

(define-signed-peeker pointer-ref-c-signed-char		sizeof-char)
(define-signed-peeker pointer-ref-c-signed-short	sizeof-short)
(define-signed-peeker pointer-ref-c-signed-int		sizeof-int)
(define-signed-peeker pointer-ref-c-signed-long		sizeof-long)
(define-signed-peeker pointer-ref-c-signed-long-long	sizeof-long-long)

(define-unsigned-peeker pointer-ref-c-unsigned-char	sizeof-char)
(define-unsigned-peeker pointer-ref-c-unsigned-short	sizeof-short)
(define-unsigned-peeker pointer-ref-c-unsigned-int	sizeof-int)
(define-unsigned-peeker pointer-ref-c-unsigned-long	sizeof-long)
(define-unsigned-peeker pointer-ref-c-unsigned-long-long sizeof-long-long)

(define pointer-ref-c-float void*-float-ref)
(define pointer-ref-c-double void*-double-ref)
(define (pointer-ref-c-void* pointer position)
  (integer->pointer (%peek-pointer (+ position (pointer->integer pointer)))))

(define pointer-ref-c-pointer pointer-ref-c-void*)


;;;; pokers

(define const:2^15 (expt 2 15))
(define const:2^16 (expt 2 16))
(define const:2^31 (expt 2 31))
(define const:2^32 (expt 2 32))
(define const:2^63 (expt 2 63))
(define const:2^64 (expt 2 64))

;;Larceny does  not provide pokers  for 64 bits  values, so we  do this.
;;They are slow, but we rarely  need 64 bits, no?  Anyway, wadda ya want
;;from me?!?
(define (%poke64 pointer value)
  (do ((i 0 (+ 1 i))
       (p pointer (+ 1 p))
       (bv (let ((bv (make-bytevector 8)))
	     (bytevector-s64-native-set! bv 0 value)
	     bv)))
      ((= i 8))
    (%poke8u p (bytevector-u8-ref bv i))))

(define (%poke64u pointer value)
  (do ((i 0 (+ 1 i))
       (p pointer (+ 1 p))
       (bv (let ((bv (make-bytevector 8)))
	     (bytevector-u64-native-set! bv 0 value)
	     bv)))
      ((= i 8))
    (%poke8u p (bytevector-u8-ref bv i))))

(define-syntax define-poker
  (syntax-rules ()
    ((_ ?name ?min ?max ?poker)
     (define (?name pointer offset value)
       (if (and (<= ?min value) (<= value ?max))
	   (?poker(+ offset (pointer->integer pointer)) value)
	 (assertion-violation (quote ?name)
	   "value out of bounds for pointer setter type" value))))))


(define-syntax define-signed-poker
  (syntax-rules ()
    ((_ ?name ?sizeof-data)
     (define ?name (case ?sizeof-data
		     ((1) pointer-set-c-int8!)
		     ((2) pointer-set-c-int16!)
		     ((4) pointer-set-c-int32!)
		     ((8) pointer-set-c-int64!))))))

(define-syntax define-unsigned-poker
  (syntax-rules ()
    ((_ ?name ?sizeof-data)
     (define ?name (case ?sizeof-data
		     ((1) pointer-set-c-uint8!)
		     ((2) pointer-set-c-uint16!)
		     ((4) pointer-set-c-uint32!)
		     ((8) pointer-set-c-uint64!))))))

(define-poker pointer-set-c-int8!	-128 127 %poke8)
(define-poker pointer-set-c-int16!	(- const:2^15) (- const:2^15 1) %poke16)
(define-poker pointer-set-c-int32!	(- const:2^31) (- const:2^31 1) %poke32)
(define-poker pointer-set-c-int64!	(- const:2^63) (- const:2^63 1) %poke64)

(define-poker pointer-set-c-uint8!	0 255 %poke8u)
(define-poker pointer-set-c-uint16!	0 (- const:2^16 1) %poke16u)
(define-poker pointer-set-c-uint32!	0 (- const:2^32 1) %poke32u)
(define-poker pointer-set-c-uint64!	0 (- const:2^64 1) %poke64u)

(define pointer-set-c-float! void*-float-set!)
(define pointer-set-c-double! void*-double-set!)

(define (pointer-set-c-void*! pointer position value)
  (%poke-pointer (+ (void*->address pointer) position)
		 (pointer->integer value)))

(define-signed-poker pointer-set-c-signed-char!		sizeof-char)
(define-signed-poker pointer-set-c-signed-short!	sizeof-short)
(define-signed-poker pointer-set-c-signed-int!		sizeof-int)
(define-signed-poker pointer-set-c-signed-long!		sizeof-long)
(define-signed-poker pointer-set-c-signed-long-long!	sizeof-long-long)

(define-unsigned-poker pointer-set-c-unsigned-char!	sizeof-char)
(define-unsigned-poker pointer-set-c-unsigned-short!	sizeof-short)
(define-unsigned-poker pointer-set-c-unsigned-int!	sizeof-int)
(define-unsigned-poker pointer-set-c-unsigned-long!	sizeof-long)
(define-unsigned-poker pointer-set-c-unsigned-long-long! sizeof-long-long)

(define pointer-set-c-pointer! pointer-set-c-void*!)


;;;; done

)

;;; end of file
