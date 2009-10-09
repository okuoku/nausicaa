;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level memory functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;	Notice that this library avoids using (foreign ffi).
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
(library (foreign memory)
  (export

    ;;conditions
    &out-of-memory			&memory-request
    make-out-of-memory-condition	make-memory-request-condition
    out-of-memory-condition?		memory-request-condition?
    condition-out-of-memory/number-of-bytes
    (rename (condition-out-of-memory/number-of-bytes
	     condition-memory-request/number-of-bytes))
    condition-memory-request/clean?
    raise-out-of-memory			raise-memory-request

    ;;memory functions
    system-free		platform-free		primitive-free
    system-malloc	system-calloc		system-realloc
    platform-malloc	platform-calloc		platform-realloc
    platform-malloc*	platform-calloc*	platform-realloc*
    primitive-malloc	primitive-calloc	primitive-realloc
    malloc		realloc			calloc

    primitive-malloc-function	primitive-calloc-function
    primitive-realloc-function	primitive-free-function

    memset		memmove		memcpy		memcmp

    ;;pointers
    pointer?
    pointer-null		pointer-null?
    integer->pointer		pointer->integer
    pointer-diff		pointer-add
    pointer=?			pointer<>?
    pointer<?			pointer>?
    pointer<=?			pointer>=?

    ;;buffers
    membuffer			membuffer*
    membuffer-used-memblock	membuffer-free-memblock!
    membuffer-incr-used-size!	membuffer-consume-bytes!
    membuffer-push-memblock!	membuffer-pop-memblock!
    membuffer-push-bytevector!	membuffer-pop-bytevector!
    membuffer-push-membuffer!

    ;;cached memory blocks
    make-block-cache		make-caching-object-factory
    small-blocks-cache		page-blocks-cache
    memblocks-cache		buffers-cache

    ;;compensated allocations
    malloc/compensated		(rename (malloc/compensated malloc/c))
    calloc/compensated		(rename (calloc/compensated calloc/c))
    malloc-small/compensated	(rename (malloc-small/compensated malloc-small/c))
    malloc-page/compensated	(rename (malloc-page/compensated malloc-page/c))
    malloc-block/compensated	(rename (malloc-block/compensated malloc-block/c))
    malloc-memblock/compensated	(rename (malloc-memblock/compensated malloc-memblock/c))
    malloc-buffer/compensated	(rename (malloc-buffer/compensated malloc-buffer/c))

    ;;bytevector conversion functions
    bytevector->pointer		pointer->bytevector
    bytevector->memblock	memblock->bytevector


    ;;buffer allocation
    memory-pool
    primitive-malloc/mempool	malloc/mempool

    ;;reference counting
    malloc/refcount		(rename (malloc/refcount malloc/rc))
    pointer-acquire		pointer-release
    pointer-dismiss

    ;;peekers
    pointer-ref-c-int8			pointer-ref-c-uint8
    pointer-ref-c-int16			pointer-ref-c-uint16
    pointer-ref-c-int32			pointer-ref-c-uint32
    pointer-ref-c-int64			pointer-ref-c-uint64
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-pointer		pointer-ref-c-void*

    ;;pokers
    pointer-set-c-int8!			pointer-set-c-uint8!
    pointer-set-c-int16!		pointer-set-c-uint16!
    pointer-set-c-int32!		pointer-set-c-uint32!
    pointer-set-c-int64!		pointer-set-c-uint64!
    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!
    pointer-set-c-pointer!		pointer-set-c-void*!

    ;;array peekers
    array-ref-c-int8			array-ref-c-uint8
    array-ref-c-int16			array-ref-c-uint16
    array-ref-c-int32			array-ref-c-uint32
    array-ref-c-int64			array-ref-c-uint64
    array-ref-c-float			array-ref-c-double
    array-ref-c-signed-char		array-ref-c-unsigned-char
    array-ref-c-signed-short		array-ref-c-unsigned-short
    array-ref-c-signed-int		array-ref-c-unsigned-int
    array-ref-c-signed-long		array-ref-c-unsigned-long
    array-ref-c-signed-long-long	array-ref-c-unsigned-long-long
    array-ref-c-void*			(rename (array-ref-c-void* array-ref-c-pointer))

    ;;array pokers
    array-set-c-int8!			array-set-c-uint8!
    array-set-c-int16!			array-set-c-uint16!
    array-set-c-int32!			array-set-c-uint32!
    array-set-c-int64!			array-set-c-uint64!
    array-set-c-float!			array-set-c-double!
    array-set-c-signed-char!		array-set-c-unsigned-char!
    array-set-c-signed-short!		array-set-c-unsigned-short!
    array-set-c-signed-int!		array-set-c-unsigned-int!
    array-set-c-signed-long!		array-set-c-unsigned-long!
    array-set-c-signed-long-long!	array-set-c-unsigned-long-long!
    array-set-c-void*!			(rename (array-set-c-void*! array-set-c-pointer!)))
  (import (nausicaa)
    (foreign memory compat)
    (foreign ffi sizeof)
    (for (foreign memory record-typedefs) expand)
    (for (foreign memory record-extensions) expand)
    (nos)
    (records)
    (sexps)
    (sexps syntax)
    (compensations))


;;;; conditions

(define-condition-type &out-of-memory &error
  make-out-of-memory-condition
  out-of-memory-condition?
  (number-of-bytes condition-out-of-memory/number-of-bytes))

(define (raise-out-of-memory who number-of-bytes)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition "out of memory")
	      (make-out-of-memory-condition number-of-bytes))))

(define-condition-type &memory-request &out-of-memory
  make-memory-request-condition
  memory-request-condition?
  (clean condition-memory-request/clean?))

(define (raise-memory-request who number-of-bytes clean)
  (raise-continuable
   (condition (make-who-condition who)
	      (make-message-condition "out of memory")
	      (make-memory-request-condition number-of-bytes clean))))


;;;; memory allocation

(define (platform-malloc* number-of-bytes)
  (let ((p (platform-malloc number-of-bytes)))
    (if (pointer-null? p) #f p)))

(define (platform-calloc* count element-size)
  (let ((p (platform-calloc count element-size)))
    (if (pointer-null? p) #f p)))

(define (platform-realloc* pointer new-size)
  (let ((p (platform-realloc pointer new-size)))
    (if (pointer-null? p) #f p)))

;;; --------------------------------------------------------------------

(define primitive-free-function
  (make-parameter platform-free
    (lambda (func)
      (assert (procedure? func))
      func)))

(define primitive-malloc-function
  (make-parameter platform-malloc*
    (lambda (func)
      (assert (procedure? func))
      func)))

(define primitive-realloc-function
  (make-parameter platform-realloc*
    (lambda (func)
      (assert (procedure? func))
      func)))

(define primitive-calloc-function
  (make-parameter platform-calloc*
    (lambda (func)
      (assert (procedure? func))
      func)))

;;; --------------------------------------------------------------------

(define (primitive-free pointer)
  ((primitive-free-function) pointer))

(define (primitive-malloc number-of-bytes)
  ((primitive-malloc-function) number-of-bytes))

(define (primitive-calloc count element-size)
  ((primitive-calloc-function) count element-size))

(define (primitive-realloc pointer new-size)
  ((primitive-realloc-function) pointer new-size))

;;; --------------------------------------------------------------------

(define (malloc number-of-bytes)
  (or (primitive-malloc number-of-bytes)
      (raise-memory-request 'malloc number-of-bytes #f)))

(define (realloc pointer new-size)
  (or (primitive-realloc pointer new-size)
      (raise-memory-request 'realloc new-size #f)))

(define (calloc count element-size)
  (or (primitive-calloc count element-size)
      (raise-memory-request 'calloc (* count element-size) #t)))


;;;; memory blocks and buffers
;;
;;The record type definitions for  <memblock> and <membuffer> are in the
;;library (foreign memory record-types).
;;

(define (memblock pointer size)
  (make <memblock>
    pointer size))

(define (membuffer pointer size)
  (make <membuffer>
    pointer size pointer 0))

(define-sexp-macro membuffer*
  ((?pointer pointer-null)
   (?size    0))
  `(,(sexp-any (sexp-or `(pointer ,(sexp-var ?pointer))
			`(size    ,(sexp-var ?size)))))
  `(let ((pointer ,?pointer)
	 (size    ,?size))
     (make <membuffer>
       pointer size pointer 0)))

(define (%membuffer-shift-to-beginning! buf)
  ;;Shift  the block of  used bytes  to the  beginning of  the allocated
  ;;block.
  ;;
  (with-record-fields (((pointer first-used used-size) <membuffer> buf))
    (when (pointer<>? pointer first-used)
      (memmove pointer first-used used-size)
      (set! first-used pointer))))

(define (membuffer-used-memblock buf)
  (with-record-fields (((first-used used-size) <membuffer> buf))
    (make <memblock> first-used used-size)))

(define (membuffer-free-memblock! buf)
  (with-virtual-fields (((free-pointer free-size) <membuffer> buf))
    (%membuffer-shift-to-beginning! buf)
    (make <memblock>
      free-pointer free-size)))

(define (membuffer-incr-used-size! buf step)
  (with-record-fields (((used-size) <membuffer> buf))
    (set! used-size (+ step used-size))))

(define (membuffer-consume-bytes! buf number-of-bytes)
  (with-record-fields (((used-size first-used pointer) <membuffer> buf))
    (set! used-size (- used-size number-of-bytes))
    (set! first-used (if (pointer=? pointer first-used)
			 pointer
		       (pointer-add first-used number-of-bytes)))))

(define (membuffer-push-memblock! buf blk)
  (with-record-fields* (((used-size first-used pointer) <membuffer> buf)
			((size pointer) <memblock> blk))
    (with-virtual-fields* (((free-pointer free-size) <membuffer> buf))
      (when (> blk.size buf.free-size)
	(%membuffer-shift-to-beginning! buf))
      (if (<= blk.size buf.free-size)
	  (begin
	    (memcpy buf.free-pointer blk.pointer blk.size)
	    (set! buf.used-size (+ buf.used-size blk.size)))
	(assertion-violation 'membuffer-push-memblock!
	  (string-append "expected source memblock with size "
			 (number->string blk.size)
			 " <= to the free size ~s in destination buffer"
			 (number->string buf.free-size))
	  (list buf blk))))))

(define (membuffer-pop-memblock! blk buf)
  (with-record-fields* (((first-used used-size) <membuffer> buf)
			((pointer size)	        <memblock>  blk))
    (if (<= blk.size buf.used-size)
	(begin
	  (memcpy blk.pointer buf.first-used blk.size)
	  (membuffer-consume-bytes! buf blk.size))
      (assertion-violation 'membuffer-pop-memblock!
	(string-append "destination memblock has size "
		       (number->string blk.size)
		       " greater than the number of available bytes "
		       (number->string buf.used-size)
		       " in source buffer")
	(list blk buf)))))

(define (membuffer-push-bytevector! buf byv)
  (with-record-fields* (((pointer size used-size) <membuffer> buf))
    (with-virtual-fields* ((free-size <membuffer> buf))
      (let-syntax ((byv.size (identifier-syntax (bytevector-length byv))))
	(when (> byv.size buf.free-size)
	  (set! buf.pointer (realloc buf.pointer (* 2 buf.size))))
	(do ((i 0 (+ 1 i)))
	    ((= i byv.size)
	     (set! buf.used-size (+ buf.used-size byv.size))
	     buf)
	  (pointer-set-c-unsigned-char! buf.pointer i (bytevector-u8-ref byv i)))))))

(define (membuffer-pop-bytevector! byv buf)
  (with-record-fields* (((pointer used-size) <membuffer> buf))
    (let-syntax ((byv.size (identifier-syntax (bytevector-length byv))))
      (when (> byv.size buf.used-size)
	(assertion-violation 'membuffer-pop-bytevector!
	  (string-append "expected destination bytevector length "
			 (number->string byv.size)
			 " <= to the free size ~s in source buffer"
			 (number->string buf.used-size))
	  (list buf byv)))
      (do ((i 0 (+ 1 i)))
	  ((= i byv.size)
	   (membuffer-consume-bytes! buf byv.size))
	(bytevector-u8-set! byv i (pointer-ref-c-unsigned-char buf.pointer i))))))

(define (membuffer-push-membuffer! dst src)
  (with-record-fields* (((pointer used-size) <membuffer> src))
    (with-virtual-fields* (((free-pointer free-size) <membuffer> dst))
      (let ((copy-size (min dst.free-size src.used-size)))
	(memcpy dst.free-pointer src.pointer copy-size)
	(membuffer-consume-bytes!  src copy-size)
	(membuffer-incr-used-size! dst copy-size)))))


;;;; caching allocated memory blocks

(define (make-block-cache block-size max-cached-block-number)
  (unless (and (fixnum? block-size)
	       (< 0 block-size))
    (assertion-violation 'make-block-cache
      "expected strictly positive fixnum as size of cached memory blocks"
      block-size))
  (unless (and (fixnum? max-cached-block-number)
	       (< 0 max-cached-block-number))
    (assertion-violation 'make-block-cache
      "expected strictly positive fixnum as max number of cached memory blocks"
      max-cached-block-number))
  (let ((list-of-cached-blocks		'())
	(number-of-cached-blocks	0))
    (case-lambda
     (()
      (if (null? list-of-cached-blocks)
	  (calloc 1 block-size)
	(let ((pointer (car list-of-cached-blocks)))
	  (set! list-of-cached-blocks (cdr list-of-cached-blocks))
	  (set! number-of-cached-blocks (- number-of-cached-blocks 1))
	  pointer)))
     ((pointer)
      (case pointer
	((purge)
	 (map primitive-free list-of-cached-blocks)
	 (set! list-of-cached-blocks '())
	 (set! number-of-cached-blocks 0))
	((list)
	 list-of-cached-blocks)
	(else
	 (if (< number-of-cached-blocks max-cached-block-number)
	     (begin
	       (set! list-of-cached-blocks (cons pointer list-of-cached-blocks))
	       (set! number-of-cached-blocks (+ 1 number-of-cached-blocks)))
	   (primitive-free pointer))))))))


;;;; caching object factories

(define (make-caching-object-factory init-func final-func
				     block-size max-cached-block-number)
  (let ((block-cache (make-block-cache block-size max-cached-block-number)))
    (case-lambda
     (()
      (let ((pointer (block-cache)))
	(init-func pointer)
	pointer))
     ((pointer)
      (case pointer
	((purge)
	 ;;Notice that  here we have ALREADY applied  the final function
	 ;;to all the stuff in the cache.
	 (block-cache 'purge))
	(else
	 (final-func pointer)
	 (block-cache pointer)))))))


;;;; predefined allocated memory blocks caches

(define small-blocks-size 32)
(define small-blocks-cache
  (make-caching-object-factory
   (lambda (x) x)
   (lambda (p) (memset p 0 small-blocks-size))
   small-blocks-size 10))

(define page-blocks-size 4096)
(define page-blocks-cache
  (make-block-cache page-blocks-size 10))

(define (memblocks-cache memblock/size)
  (cond ((is-a? memblock/size <memblock>)
	 (with-record-fields (((pointer size) <memblock> memblock/size))
	   (cond
	    ((= size small-blocks-size)
	     (small-blocks-cache pointer))
	    ((= size page-blocks-size)
	     (page-blocks-cache pointer))
	    (else
	     (primitive-free pointer)))))
	((<= memblock/size small-blocks-size)
	 (make <memblock> (small-blocks-cache) small-blocks-size))
	((<= memblock/size page-blocks-size)
	 (make <memblock> (page-blocks-cache) page-blocks-size))
	(else
	 (make <memblock> (malloc memblock/size) memblock/size))))

(define (buffers-cache membuffer/size)
  (cond ((is-a? membuffer/size <membuffer>)
	 (with-record-fields (((pointer size) <membuffer> membuffer/size))
	   (case size
	     ((sall-blocks-size)
	      (small-blocks-cache pointer))
	     ((page-blocks-size)
	      (page-blocks-cache pointer))
	     (else
	      (primitive-free pointer)))))
	((<= membuffer/size small-blocks-size)
	 (membuffer (small-blocks-cache) small-blocks-size 0))
	((<= membuffer/size page-blocks-size)
	 (membuffer (page-blocks-cache) page-blocks-size 0))
	(else
	 (membuffer (malloc membuffer/size) membuffer/size 0))))


;;;; compensated allocations

(define (malloc/compensated number-of-bytes)
  (letrec
      ((p (compensate
	      (malloc number-of-bytes)
	    (with
	     (primitive-free p)))))
    p))

(define (calloc/compensated count element-size)
  (letrec
      ((p (compensate
	      (calloc count element-size)
	    (with
	     (primitive-free p)))))
    p))

;;; --------------------------------------------------------------------

(define (malloc-small/compensated)
  (letrec
      ((p (compensate
	      (small-blocks-cache)
	    (with
	     (small-blocks-cache p)))))
    p))

(define (malloc-page/compensated)
  (letrec
      ((p (compensate
	      (page-blocks-cache)
	    (with
	     (page-blocks-cache p)))))
    p))

(define (malloc-block/compensated number-of-bytes)
  (cond
   ((<= number-of-bytes small-blocks-size)
    (malloc-small/compensated))
   ((<= number-of-bytes small-blocks-size)
    (malloc-page/compensated))
   (else
    (calloc/compensated 1 number-of-bytes))))

(define (malloc-memblock/compensated number-of-bytes)
  (letrec
      ((mb (compensate
	       (memblocks-cache number-of-bytes)
	     (with
	      (memblocks-cache mb)))))
    mb))

(define (malloc-buffer/compensated number-of-bytes)
  (letrec
      ((buffer (compensate
		   (buffers-cache number-of-bytes)
		 (with
		  (buffers-cache buffer)))))
    buffer))


;;;; allocation in buffers

(define memory-pool
  (make-parameter #f
    (lambda (obj)
      (unless (or (not obj) (is-a? obj <mempool>))
	(assertion-violation 'memory-buffer-pool
	  "expected #f or memory pool as parameter value" obj))
      obj)))

(define (primitive-malloc/mempool number-of-bytes)
  (let ((pool (memory-pool)))
    (assert pool)
    (with-record-fields ((next <mempool> pool))
      (with-virtual-fields ((free-size <mempool> pool))
	(and (< number-of-bytes free-size)
	     (begin0
		 next
	       (set! next (pointer-add next number-of-bytes))))))))

(define (malloc/mempool number-of-bytes)
  (or (primitive-malloc/mempool number-of-bytes)
      (raise-out-of-memory 'malloc/mempool number-of-bytes)))


;;;; reference counting

(define malloc/refcount
  (case-lambda
   ((number-of-bytes)
    (malloc/refcount number-of-bytes malloc))
   ((number-of-bytes malloc-funk)
    (let ((p (malloc-funk (+ strideof-long number-of-bytes))))
      (pointer-set-c-unsigned-long! p 0 0)
      (pointer-add p strideof-long)))))

(define-syntax refcount-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (pointer-set-c-unsigned-long! ?pointer (- strideof-long) ?value))))

(define-syntax refcount-ref
  (syntax-rules ()
    ((_ ?pointer)
     (pointer-ref-c-unsigned-long ?pointer (- strideof-long)))))

(define (pointer-acquire pointer)
  (refcount-set! pointer (+ 1 (refcount-ref pointer))))

(define (pointer-refcount-begin pointer)
  (pointer-add pointer (- strideof-long)))

(define pointer-release
  (case-lambda
   ((pointer)
    (pointer-release pointer primitive-free))
   ((pointer free-func)
    (let ((rc (refcount-ref pointer)))
      (if (= 1 rc)
	  (primitive-free (pointer-refcount-begin pointer))
	(refcount-set! pointer (- rc 1)))))))

(define pointer-dismiss
  (case-lambda
   ((pointer)
    (pointer-dismiss pointer primitive-free))
   ((pointer free-func)
    (primitive-free (pointer-refcount-begin pointer)))))


;;;; bytevector conversion functions

(define bytevector->pointer
  (case-lambda
   ((bv malloc)
    (bytevector->pointer bv malloc (bytevector-length bv) 0))
   ((bv malloc number-of-bytes)
    (bytevector->pointer bv malloc number-of-bytes 0))
   ((bv malloc number-of-bytes offset)
    (let* ((p	(malloc number-of-bytes)))
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-bytes)
	   p)
	(pointer-set-c-uint8! p i (bytevector-u8-ref bv (+ i offset))))))))

(define bytevector->memblock
  (case-lambda
   ((bv malloc)
    (bytevector->memblock bv malloc (bytevector-length bv) 0))
   ((bv malloc number-of-bytes)
    (bytevector->memblock bv malloc number-of-bytes 0))
   ((bv malloc number-of-bytes offset)
    (make <memblock>
      (bytevector->pointer bv malloc number-of-bytes offset)
      number-of-bytes))))

;;; --------------------------------------------------------------------

(define pointer->bytevector
  (case-lambda
   ((pointer number-of-bytes)
    (pointer->bytevector pointer number-of-bytes 0))
   ((pointer number-of-bytes offset)
    (let* ((bv (make-bytevector number-of-bytes)))
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-bytes)
	   bv)
	(bytevector-u8-set! bv i (pointer-ref-c-uint8 pointer (+ i offset))))))))

(define memblock->bytevector
  (case-lambda
   ((blk)
    (with-record-fields ((size <memblock> blk))
      (memblock->bytevector blk size 0)))
   ((blk number-of-bytes)
    (memblock->bytevector blk number-of-bytes 0))
   ((blk number-of-bytes offset)
    (with-record-fields ((pointer <memblock> blk))
      (pointer->bytevector pointer number-of-bytes offset)))))


;;;; array peekers

(let-syntax ((define-array-peeker (syntax-rules ()
				    ((_ ?name ?peeker ?strideof-data)
				     (define (?name pointer index)
				       (?peeker pointer (* index ?strideof-data))))
				    ((_ ?name ?peeker ?strideof-data ?mapper)
				     (define (?name pointer index)
				       (?mapper (?peeker pointer (* index ?strideof-data))))))))
  (define-array-peeker array-ref-c-int8		pointer-ref-c-int8	1)
  (define-array-peeker array-ref-c-int16	pointer-ref-c-int16	2)
  (define-array-peeker array-ref-c-int32	pointer-ref-c-int32	4)
  (define-array-peeker array-ref-c-int64	pointer-ref-c-int64	8)

  (define-array-peeker array-ref-c-uint8	pointer-ref-c-uint8	1)
  (define-array-peeker array-ref-c-uint16	pointer-ref-c-uint16	2)
  (define-array-peeker array-ref-c-uint32	pointer-ref-c-uint32	4)
  (define-array-peeker array-ref-c-uint64	pointer-ref-c-uint64	8)

  (define-array-peeker array-ref-c-float	pointer-ref-c-float	strideof-float)
  (define-array-peeker array-ref-c-double	pointer-ref-c-double	strideof-float)
  (define-array-peeker array-ref-c-void*	pointer-ref-c-void*	strideof-pointer))

(let-syntax ((define-signed-array-peeker (syntax-rules ()
					   ((_ ?name ?sizeof-data)
					    (define ?name (case ?sizeof-data
							    ((1) array-ref-c-int8)
							    ((2) array-ref-c-int16)
							    ((4) array-ref-c-int32)
							    ((8) array-ref-c-int64)))))))
  (define-signed-array-peeker array-ref-c-signed-char		sizeof-char)
  (define-signed-array-peeker array-ref-c-signed-short		sizeof-short)
  (define-signed-array-peeker array-ref-c-signed-int		sizeof-int)
  (define-signed-array-peeker array-ref-c-signed-long		sizeof-long)
  (define-signed-array-peeker array-ref-c-signed-long-long	sizeof-long-long))

(let-syntax ((define-unsigned-array-peeker (syntax-rules ()
					     ((_ ?name ?sizeof-data)
					      (define ?name (case ?sizeof-data
							      ((1) array-ref-c-uint8)
							      ((2) array-ref-c-uint16)
							      ((4) array-ref-c-uint32)
							      ((8) array-ref-c-uint64)))))))
  (define-unsigned-array-peeker array-ref-c-unsigned-char	sizeof-char)
  (define-unsigned-array-peeker array-ref-c-unsigned-short	sizeof-short)
  (define-unsigned-array-peeker array-ref-c-unsigned-int	sizeof-int)
  (define-unsigned-array-peeker array-ref-c-unsigned-long	sizeof-long)
  (define-unsigned-array-peeker array-ref-c-unsigned-long-long	sizeof-long-long))


;;;; array pokers

(let-syntax ((define-array-poker (syntax-rules ()
				   ((_ ?name ?poker ?strideof-data)
				    (define (?name pointer index value)
				      (?poker pointer (* index ?strideof-data) value)))
				   ((_ ?name ?poker ?strideof-data ?mapper)
				    (define (?name pointer index value)
				      (?poker pointer (* index ?strideof-data) (?mapper value)))))))
  (define-array-poker array-set-c-int8!		pointer-set-c-int8!	1)
  (define-array-poker array-set-c-int16!	pointer-set-c-int16!	2)
  (define-array-poker array-set-c-int32!	pointer-set-c-int32!	4)
  (define-array-poker array-set-c-int64!	pointer-set-c-int64!	8)

  (define-array-poker array-set-c-uint8!	pointer-set-c-uint8!	1)
  (define-array-poker array-set-c-uint16!	pointer-set-c-uint16!	2)
  (define-array-poker array-set-c-uint32!	pointer-set-c-uint32!	4)
  (define-array-poker array-set-c-uint64!	pointer-set-c-uint64!	8)

  (define-array-poker array-set-c-float!	pointer-set-c-float!	strideof-float)
  (define-array-poker array-set-c-double!	pointer-set-c-double!	strideof-float)
  (define-array-poker array-set-c-void*!	pointer-set-c-void*!	strideof-pointer))

(let-syntax ((define-signed-array-poker (syntax-rules ()
					  ((_ ?name ?sizeof-data)
					   (define ?name (case ?sizeof-data
							   ((1) array-set-c-int8!)
							   ((2) array-set-c-int16!)
							   ((4) array-set-c-int32!)
							   ((8) array-set-c-int64!)))))))
  (define-signed-array-poker array-set-c-signed-char!		sizeof-char)
  (define-signed-array-poker array-set-c-signed-short!		sizeof-short)
  (define-signed-array-poker array-set-c-signed-int!		sizeof-int)
  (define-signed-array-poker array-set-c-signed-long!		sizeof-long)
  (define-signed-array-poker array-set-c-signed-long-long!	sizeof-long-long))

(let-syntax ((define-unsigned-array-poker (syntax-rules ()
					    ((_ ?name ?sizeof-data)
					     (define ?name (case ?sizeof-data
							     ((1) array-set-c-uint8!)
							     ((2) array-set-c-uint16!)
							     ((4) array-set-c-uint32!)
							     ((8) array-set-c-uint64!)))))))
  (define-unsigned-array-poker array-set-c-unsigned-char!	sizeof-char)
  (define-unsigned-array-poker array-set-c-unsigned-short!	sizeof-short)
  (define-unsigned-array-poker array-set-c-unsigned-int!	sizeof-int)
  (define-unsigned-array-poker array-set-c-unsigned-long!	sizeof-long)
  (define-unsigned-array-poker array-set-c-unsigned-long-long!	sizeof-long-long))


;;;; done

)

;;; end of file
