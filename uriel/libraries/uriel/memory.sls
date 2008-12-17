;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: low level memory functions
;;;Date: Tue Dec 16, 2008
;;;Time-stamp: <2008-12-17 13:53:26 marco>
;;;
;;;Abstract
;;;
;;;	Notice that this library avoids using (uriel ffi).
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(library (uriel memory)
  (export
    ;;memory functions
    platform-free	primitive-free		primitive-free-function
    platform-malloc	primitive-malloc	primitive-malloc-function
    platform-calloc	primitive-calloc	primitive-calloc-function
    platform-realloc    primitive-realloc	primitive-realloc-function

    malloc		realloc			calloc
    memset		memmove			memcpy

    ;;pointers
    pointer?
    pointer-null		pointer-null?
    integer->pointer		pointer->integer
    pointer-diff		pointer-add

    pointer=?			pointer<>?
    pointer<?			pointer>?
    pointer<=?			pointer>=?

    ;;memory blocks
    make-memblock		memblock?
    memblock-pointer		memblock-size

    ;;buffers
    make-buffer
    (rename (memblock-pointer	buffer-pointer))
    (rename (memblock-size	buffer-size))
    buffer?				buffer-used?
    buffer-full?			buffer-empty?
    buffer-used-size			buffer-used-size-set!
    buffer-free-size
    buffer-pointer-to-free-bytes	buffer-incr-used-size
    buffer-used-memblock		buffer-free-memblock
    buffer-push-memblock		buffer-pop-memblock
    buffer-push-bytevector		buffer-pop-bytevector

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
    bytevector->pointer bytevector->memblock
    pointer->bytevector memblock->bytevector

    ;;peekers
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-char!			pointer-set-c-short!
    pointer-set-c-int!			pointer-set-c-long!
    pointer-set-c-long-long!		pointer-set-c-float!
    pointer-set-c-double!		pointer-set-c-pointer!

    ;;conditions
    &out-of-memory
    make-out-of-memory-condition	out-of-memory-condition?
    out-of-memory-number-of-bytes	raise-out-of-memory)
  (import (r6rs)
    (uriel lang)
    (uriel memory compat)
    (srfi format)
    (srfi parameters))



;;;; memory allocation

(define primitive-free-function
  (make-parameter platform-free
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-free-function
	  "expected function as parameter value" func))
      func)))

(define primitive-malloc-function
  (make-parameter platform-malloc
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-malloc-function
	  "expected function as parameter value" func))
      func)))

(define primitive-realloc-function
  (make-parameter platform-realloc
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-realloc-function
	  "expected function as parameter value" func))
      func)))

(define primitive-calloc-function
  (make-parameter platform-calloc
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-calloc-function
	  "expected function as parameter value" func))
      func)))

;;; --------------------------------------------------------------------

(define (primitive-free pointer)
  ((primitive-free-function) pointer))

(define (primitive-malloc number-of-bytes)
  (let ((p ((primitive-malloc-function) number-of-bytes)))
    (if (pointer-null? p) #f p)))

(define (primitive-calloc count element-size)
  (let ((p ((primitive-calloc-function) count element-size)))
    (if (pointer-null? p) #f p)))

(define (primitive-realloc pointer new-size)
  (let ((p ((primitive-realloc-function) pointer new-size)))
    (if (pointer-null? p) #f p)))

;;; --------------------------------------------------------------------

(define (malloc number-of-bytes)
  (or (primitive-malloc number-of-bytes)
      (raise-out-of-memory 'malloc number-of-bytes)))

(define (realloc pointer new-size)
  (or (primitive-realloc pointer new-size)
      (raise-out-of-memory 'realloc new-size)))

(define (calloc count element-size)
  (or (primitive-calloc count element-size)
      (raise-out-of-memory 'calloc (* count element-size))))



;;;; pointer functions

(define (pointer-diff pointer-1 pointer-2)
  (- (pointer->integer pointer-1)
     (pointer->integer pointer-2)))

(define (pointer-add pointer offset)
  (integer->pointer (+ (pointer->integer pointer)
		       offset)))

(define pointer-null
  (integer->pointer 0))

(define (pointer-null? pointer)
  (= 0 (pointer->integer pointer)))

(define (pointer=? pointer . args)
  (apply = (map pointer->integer (cons pointer args))))

(define (pointer<? pointer . args)
  (apply < (map pointer->integer (cons pointer args))))

(define (pointer>? pointer . args)
  (apply > (map pointer->integer (cons pointer args))))

(define (pointer<=? pointer . args)
  (apply <= (map pointer->integer (cons pointer args))))

(define (pointer>=? pointer . args)
  (apply >= (map pointer->integer (cons pointer args))))

(define (pointer<>? pointer . args)
  (not (apply pointer=? pointer args)))



;;;; records

(define-record-type memblock
  (fields (immutable pointer)
	  (immutable size)))

(define-record-type buffer
  (parent memblock)
  (fields (mutable used-size)))


(define (buffer-empty? buf)
  (= 0 (buffer-used-size buf)))

(define (buffer-full? buf)
  (= (memblock-size    buf)
     (buffer-used-size buf)))

(define (buffer-used? buf)
  (not (= 0 (buffer-used-size buf))))

(define (buffer-free-size buf)
  (- (memblock-size    buf)
     (buffer-used-size buf)))

(define (buffer-pointer-to-free-bytes buf)
  (pointer-add (memblock-pointer buf)
	       (buffer-used-size buf)))

(define (buffer-used-memblock buf)
  (make-memblock (memblock-pointer buf)
		 (buffer-used-size buf)))

(define (buffer-free-memblock buf)
  (make-memblock (buffer-pointer-to-free-bytes buf)
		 (buffer-free-size  buf)))

(define (buffer-incr-used-size buf step)
  (buffer-used-size-set! buf
			 (+ step (buffer-used-size buf))))

(define (buffer-push-memblock dst-buf src-mb)
  (let ((push-len (memblock-size src-mb))
	(free-len (buffer-free-size dst-buf)))
    (when (> push-len free-len)
      (assertion-violation 'buffer-push-block
	(format
	    "expected source memblock with size ~s <= to the free size ~s in destination buffer"
	  push-len free-len)
	(list dst-buf src-mb)))
    (memcpy (buffer-pointer-to-free-bytes dst-buf)
	    (memblock-pointer src-mb)
	    push-len)
    (buffer-incr-used-size dst-buf push-len)))

(define (buffer-pop-memblock dst-mb src-buf)
  (let* ((src-ptr	(memblock-pointer src-buf))
	 (dst-ptr	(memblock-pointer dst-mb))
	 (used-size	(buffer-used-size src-buf))
	 (pop-len	(memblock-size    dst-mb))
	 (shift-len	(- used-size pop-len)))
    (when (> pop-len used-size)
      (assertion-violation 'buffer-pop-block
	(format
	    "expected destination memblock with size ~s <= to the used size ~s in source buffer"
	  pop-len used-size)
	(list dst-mb src-buf)))
    (memcpy  dst-ptr src-ptr pop-len)
    (memmove src-ptr (pointer-add src-ptr pop-len) shift-len)
    (buffer-incr-used-size src-buf (- pop-len))))

(define (buffer-push-bytevector dst-buf src-bv)
  #f)

(define (buffer-pop-bytevector dst-bv src-buf)
  #f)

(define (buffer-push-buffer dst-buf src-buf)
  #f)

(define (buffer-pop-buffer dst-buf src-buf)
  #f)


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

(define (memblocks-cache memblock-or-size)
  (cond
   ((memblock? memblock-or-size)
    (let ((size		(memblock-size memblock-or-size))
	  (pointer	(memblock-pointer memblock-or-size)))
      (cond
       ((= size small-blocks-size)
	(small-blocks-cache pointer))
       ((= size page-blocks-size)
	(page-blocks-cache pointer))
       (else
	(primitive-free pointer)))))
   ((<= memblock-or-size small-blocks-size)
    (make-memblock (small-blocks-cache) small-blocks-size))
   ((<= memblock-or-size page-blocks-size)
    (make-memblock (page-blocks-cache) page-blocks-size))
   (else
    (make-memblock (malloc memblock-or-size) memblock-or-size))))

(define (buffers-cache buffer-or-size)
  (cond
   ((buffer? buffer-or-size)
    (let ((size		(memblock-size buffer-or-size))
	  (pointer	(memblock-pointer buffer-or-size)))
      (case size
       ((sall-blocks-size)
	(small-blocks-cache pointer))
       ((page-blocks-size)
	(page-blocks-cache pointer))
       (else
	(primitive-free pointer)))))
   ((<= buffer-or-size small-blocks-size)
    (make-buffer (small-blocks-cache) small-blocks-size 0))
   ((<= buffer-or-size page-blocks-size)
    (make-buffer (page-blocks-cache) page-blocks-size 0))
   (else
    (make-buffer (malloc buffer-or-size) buffer-or-size 0))))



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
   ((<= small-blocks-size number-of-bytes)
    (malloc-small/compensated))
   ((<= small-blocks-size number-of-bytes)
    (malloc-page/compensated))
   (else
    (malloc/compensated number-of-bytes))))

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
	(pointer-set-c-char! p i (bytevector-u8-ref bv (+ i offset))))))))

(define bytevector->memblock
  (case-lambda
   ((bv malloc)
    (bytevector->memblock bv malloc (bytevector-length bv) 0))
   ((bv malloc number-of-bytes)
    (bytevector->memblock bv malloc number-of-bytes 0))
   ((bv malloc number-of-bytes offset)
    (make-memblock
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
	(bytevector-u8-set! bv i (pointer-ref-c-unsigned-char pointer (+ i offset))))))))

(define memblock->bytevector
  (case-lambda
   ((mb)
    (memblock->bytevector mb (memblock-size mb) 0))
   ((mb number-of-bytes)
    (memblock->bytevector mb number-of-bytes 0))
   ((mb number-of-bytes offset)
    (pointer->bytevector (memblock-pointer mb) number-of-bytes offset))))



;;;; conditions

(define-condition-type &out-of-memory &error
  make-out-of-memory-condition out-of-memory-condition?
  (number-of-bytes out-of-memory-number-of-bytes))

(define (raise-out-of-memory who number-of-bytes)
  (raise (condition (make-who-condition who)
		    (make-message-condition "out of memory")
		    (make-out-of-memory-condition number-of-bytes))))



;;;; done

)

;;; end of file
