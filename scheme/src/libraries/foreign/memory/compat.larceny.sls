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



#!r6rs
(library (foreign memory compat)
  (export

    system-free				system-malloc
    system-calloc			system-realloc

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
    (rename (void*-float-ref	pointer-ref-c-float)
	    (void*-double-ref	pointer-ref-c-double))
    pointer-ref-c-void*

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    (rename (pointer-ref-c-void* pointer-ref-c-pointer))

    ;;pokers
    pointer-set-c-int8!			pointer-set-c-uint8!
    pointer-set-c-int16!		pointer-set-c-uint16!
    pointer-set-c-int32!		pointer-set-c-uint32!
    pointer-set-c-int64!		pointer-set-c-uint64!
    (rename (void*-float-set!	pointer-set-c-float!)
	    (void*-double-set!	pointer-set-c-double!))
    pointer-set-c-void*!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!
    (rename (pointer-set-c-void*! pointer-set-c-pointer!)))
  (import (rnrs)
    (primitives foreign-procedure
		%peek8 %peek8u %peek16 %peek16u %peek32 %peek32u %peek-pointer
		%poke8 %poke8u %poke16 %poke16u %poke32 %poke32u %poke-pointer
		void*-double-set! void*-double-ref void*-float-set! void*-float-ref
		void*? void*-rt record-constructor void*->address)
    (foreign ffi sizeof))


;;;; pointers
;;
;;Larceny handles pointers  records of type "void*", but  when a pointer
;;is  internally  detected  to be  NULL  it  is  converted to  #f.   The
;;following is a basic guide on the "void*" API:
;;
;; void*-rt	Bound to the record type descriptor.
;;
;; void*?	Return true when applied to a "void*" record.  This
;;		predicate does NOT accept #f as valid value.
;;
;; void*->address
;;		Applied to a "void*" record return an exact integer
;;		representing the memory address.
;;
;; record-constructor
;;		Applied to a record type descriptor returns the record
;;		constructor procedure.  The record constructor is raw,
;;		it does NOT validate its arguments.
;;
;;The following interface provides what is needed to export bindings for
;;"void*" aliases to  bindings for a virtual "pointer"  type.  The macro
;;RETVAL->POINTER  is to  be used  to convert  to a  "void*"  record the
;;return value of the callout procedures (see below).
;;

(define make-pointer (record-constructor void*-rt))

(define pointer? void*?)

(define (integer->pointer value)
  (if (integer? value)
      (make-pointer value)
    (assertion-violation 'integer->pointer
      "expected integer value" value)))

(define (pointer->integer pointer)
  (if (pointer? pointer)
      (void*->address pointer)
    (assertion-violation 'pointer->integer
      "expected pointer value" pointer)))

(define-syntax retval->pointer
  (syntax-rules ()
    ((_ ?value)
     (let ((value ?value))
       (if (void*? value)
	   value
	 pointer-null)))))

(define pointer-null
  (make-pointer 0))

(define (pointer-null? pointer)
  (zero? (void*->address pointer)))

(define (pointer-diff pointer-1 pointer-2)
  (- (pointer->integer pointer-1)
     (pointer->integer pointer-2)))

(define (pointer-add pointer offset)
  (integer->pointer (+ (pointer->integer pointer)
		       offset)))

(let-syntax ((define-pointer-comparison (syntax-rules ()
					  ((_ ?name ?func)
					   (define ?name
					     (case-lambda
					      (()		#f)
					      ((pointer)	#t)
					      ((pointer-a pointer-b)
					       (?func (pointer->integer pointer-a)
						      (pointer->integer pointer-b)))
					      ((pointer-a pointer-b . pointers)
					       (apply ?func (map pointer->integer
							      (cons* pointer-a
								     pointer-b pointers))))))))))
  (define-pointer-comparison pointer=? =)
  (define-pointer-comparison pointer<? <)
  (define-pointer-comparison pointer>? >)
  (define-pointer-comparison pointer<=? <=)
  (define-pointer-comparison pointer>=? >=))

(define pointer<>?
  (case-lambda
   (()
    #f)
   ((pointer . pointers)
    (not (apply pointer=? pointer pointers)))))


;;;; low level allocation functions
;;
;;For the "system-" functions we want pointers to be exact integers; for
;;the  "platform-" functions  we want  pointers  to be  records of  type
;;"pointer".
;;
;;See above for a discussion of pointer values in Larceny.
;;

(define platform-free
  (foreign-procedure "free" '(void*) 'void))

(define platform-malloc
  (let ((f (foreign-procedure "malloc" '(unsigned) 'void*)))
    (lambda (number-of-bytes)
      (retval->pointer (f number-of-bytes)))))

(define platform-realloc
  (let ((f (foreign-procedure "realloc" '(void* unsigned) 'void*)))
    (lambda (pointer number-of-bytes)
      (retval->pointer (f pointer number-of-bytes)))))

(define platform-calloc
  (let ((f (foreign-procedure "calloc" '(unsigned unsigned) 'void*)))
    (lambda (count element-size)
      (retval->pointer (f count element-size)))))

;;; --------------------------------------------------------------------

(define (system-free pointer-integer)
  (platform-free (integer->pointer pointer-integer)))

(define (system-malloc number-of-bytes)
  (pointer->integer (platform-malloc number-of-bytes)))

(define (system-realloc pointer-integer number-of-bytes)
  (pointer->integer (platform-realloc (integer->pointer pointer-integer)
				      number-of-bytes)))

(define (system-calloc count element-size)
  (pointer->integer (platform-calloc count element-size)))


;;;; low level memory operations

(define memset
  (foreign-procedure "memset" '(void* int unsigned) 'void*))

(define memmove
  (foreign-procedure "memmove" '(void* void* unsigned) 'void*))

(define memcpy
  (foreign-procedure "memcpy" '(void* void* unsigned) 'void*))

(define memcmp
  (foreign-procedure "memcmp" '(void* void* unsigned) 'int))



;;;; low level peekers and pokers
;;
;;Larceny does  not provide peekers for  64 bits values, so  we do this.
;;They are slow, but we rarely  need 64 bits, no?  Anyway, wadda ya want
;;from me?!?
;;

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


;;;; peekers

(let-syntax ((define-peeker (syntax-rules ()
			      ((_ ?name ?peeker)
			       (define (?name pointer position)
				 (?peeker (+ position (pointer->integer pointer))))))))
  (define-peeker pointer-ref-c-int8	%peek8)
  (define-peeker pointer-ref-c-int16	%peek16)
  (define-peeker pointer-ref-c-int32	%peek32)
  (define-peeker pointer-ref-c-int64	%peek64)

  (define-peeker pointer-ref-c-uint8	%peek8u)
  (define-peeker pointer-ref-c-uint16	%peek16u)
  (define-peeker pointer-ref-c-uint32	%peek32u)
  (define-peeker pointer-ref-c-uint64	%peek64u))

(let-syntax ((define-signed-peeker (syntax-rules ()
				     ((_ ?name ?sizeof-data)
				      (define ?name (case ?sizeof-data
						      ((1) pointer-ref-c-int8)
						      ((2) pointer-ref-c-int16)
						      ((4) pointer-ref-c-int32)
						      ((8) pointer-ref-c-int64)))))))
  (define-signed-peeker pointer-ref-c-signed-char	sizeof-char)
  (define-signed-peeker pointer-ref-c-signed-short	sizeof-short)
  (define-signed-peeker pointer-ref-c-signed-int	sizeof-int)
  (define-signed-peeker pointer-ref-c-signed-long	sizeof-long)
  (define-signed-peeker pointer-ref-c-signed-long-long	sizeof-long-long))

(let-syntax ((define-unsigned-peeker (syntax-rules ()
				       ((_ ?name ?sizeof-data)
					(define ?name (case ?sizeof-data
							((1) pointer-ref-c-uint8)
							((2) pointer-ref-c-uint16)
							((4) pointer-ref-c-uint32)
							((8) pointer-ref-c-uint64)))))))
  (define-unsigned-peeker pointer-ref-c-unsigned-char		sizeof-char)
  (define-unsigned-peeker pointer-ref-c-unsigned-short		sizeof-short)
  (define-unsigned-peeker pointer-ref-c-unsigned-int		sizeof-int)
  (define-unsigned-peeker pointer-ref-c-unsigned-long		sizeof-long)
  (define-unsigned-peeker pointer-ref-c-unsigned-long-long	sizeof-long-long))

(define (pointer-ref-c-void* pointer position)
  (integer->pointer (%peek-pointer (+ position (pointer->integer pointer)))))


;;;; pokers

(define const:2^15 (expt 2 15))
(define const:2^16 (expt 2 16))
(define const:2^31 (expt 2 31))
(define const:2^32 (expt 2 32))
(define const:2^63 (expt 2 63))
(define const:2^64 (expt 2 64))

(let-syntax ((define-poker (syntax-rules ()
			     ((_ ?name ?min ?max ?poker)
			      (define (?name pointer offset value)
				(if (and (<= ?min value) (<= value ?max))
				    (?poker (+ offset (pointer->integer pointer)) value)
				  (assertion-violation (quote ?name)
				    "value out of bounds for pointer setter type" value)))))))
  (define-poker pointer-set-c-int8!	-128 127 %poke8)
  (define-poker pointer-set-c-int16!	(- const:2^15) (- const:2^15 1) %poke16)
  (define-poker pointer-set-c-int32!	(- const:2^31) (- const:2^31 1) %poke32)
  (define-poker pointer-set-c-int64!	(- const:2^63) (- const:2^63 1) %poke64)

  (define-poker pointer-set-c-uint8!	0 255 %poke8u)
  (define-poker pointer-set-c-uint16!	0 (- const:2^16 1) %poke16u)
  (define-poker pointer-set-c-uint32!	0 (- const:2^32 1) %poke32u)
  (define-poker pointer-set-c-uint64!	0 (- const:2^64 1) %poke64u))

(let-syntax ((define-signed-poker (syntax-rules ()
				    ((_ ?name ?sizeof-data)
				     (define ?name (case ?sizeof-data
						     ((1) pointer-set-c-int8!)
						     ((2) pointer-set-c-int16!)
						     ((4) pointer-set-c-int32!)
						     ((8) pointer-set-c-int64!)))))))
  (define-signed-poker pointer-set-c-signed-char!	sizeof-char)
  (define-signed-poker pointer-set-c-signed-short!	sizeof-short)
  (define-signed-poker pointer-set-c-signed-int!	sizeof-int)
  (define-signed-poker pointer-set-c-signed-long!	sizeof-long)
  (define-signed-poker pointer-set-c-signed-long-long!	sizeof-long-long))

(let-syntax ((define-unsigned-poker (syntax-rules ()
				      ((_ ?name ?sizeof-data)
				       (define ?name (case ?sizeof-data
						       ((1) pointer-set-c-uint8!)
						       ((2) pointer-set-c-uint16!)
						       ((4) pointer-set-c-uint32!)
						       ((8) pointer-set-c-uint64!)))))))
  (define-unsigned-poker pointer-set-c-unsigned-char!		sizeof-char)
  (define-unsigned-poker pointer-set-c-unsigned-short!		sizeof-short)
  (define-unsigned-poker pointer-set-c-unsigned-int!		sizeof-int)
  (define-unsigned-poker pointer-set-c-unsigned-long!		sizeof-long)
  (define-unsigned-poker pointer-set-c-unsigned-long-long!	sizeof-long-long))

(define (pointer-set-c-void*! pointer position value)
  (%poke-pointer (+ (void*->address pointer) position)
		 (pointer->integer value)))


;;;; done

)

;;; end of file
