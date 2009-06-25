;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level memory functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;	Notice that this library avoids using (uriel ffi).
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

(library (foreign memory)
  (export
    ;;memory functions
    platform-free	primitive-free		primitive-free-function
    platform-malloc	primitive-malloc	primitive-malloc-function
    platform-calloc	primitive-calloc	primitive-calloc-function
    platform-realloc    primitive-realloc	primitive-realloc-function

    malloc		realloc			calloc
    memset		memmove			memcpy
    memcmp

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
    buffer-free-size			buffer-consume-bytes!
    buffer-used-memblock		buffer-free-memblock
    buffer-pointer-to-free-bytes	buffer-incr-used-size!
    buffer-push-memblock!		buffer-pop-memblock!
    buffer-push-bytevector!		buffer-pop-bytevector!
    buffer-push-buffer!
    (rename (buffer-push-buffer! buffer-pop-buffer!))

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
    memory-buffer-pool
    primitive-buffer-malloc	buffer-malloc

    ;;reference counting
    malloc/refcount		(rename (malloc/refcount malloc/rc))
    pointer-acquire		pointer-release
    pointer-dismiss

    ;;peekers
    pointer-ref-c-signed-char			pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short			pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int			pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long			pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long		pointer-ref-c-unsigned-long-long
    pointer-ref-c-float				pointer-ref-c-double
    pointer-ref-c-pointer

    (rename (pointer-ref-c-signed-char		peek-signed-char))
    (rename (pointer-ref-c-unsigned-char	peek-unsigned-char))
    (rename (pointer-ref-c-signed-short		peek-signed-short))
    (rename (pointer-ref-c-unsigned-short	peek-unsigned-short))
    (rename (pointer-ref-c-signed-int		peek-signed-int))
    (rename (pointer-ref-c-unsigned-int		peek-unsigned-int))
    (rename (pointer-ref-c-signed-long		peek-signed-long))
    (rename (pointer-ref-c-unsigned-long	peek-unsigned-long))
    (rename (pointer-ref-c-signed-long-long	peek-signed-long-long))
    (rename (pointer-ref-c-unsigned-long-long	peek-unsigned-long-long))
    (rename (pointer-ref-c-float		peek-float))
    (rename (pointer-ref-c-double		peek-double))
    (rename (pointer-ref-c-pointer		peek-pointer))

    ;;pokers
    pointer-set-c-char!				pointer-set-c-short!
    pointer-set-c-int!				pointer-set-c-long!
    pointer-set-c-long-long!			pointer-set-c-float!
    pointer-set-c-double!			pointer-set-c-pointer!

    (rename (pointer-set-c-char!		poke-char!))
    (rename (pointer-set-c-short!		poke-short!))
    (rename (pointer-set-c-int!			poke-int!))
    (rename (pointer-set-c-long!		poke-long!))
    (rename (pointer-set-c-long-long!		poke-long-long!))
    (rename (pointer-set-c-float!		poke-float!))
    (rename (pointer-set-c-double!		poke-double!))
    (rename (pointer-set-c-pointer!		poke-pointer!))

    ;;array peekers
    (rename (pointer-ref-c-signed-char   peek-array-signed-char))
    (rename (pointer-ref-c-unsigned-char peek-array-unsigned-char))
    peek-array-signed-short		peek-array-unsigned-short
    peek-array-signed-int		peek-array-unsigned-int
    peek-array-signed-long		peek-array-unsigned-long
    peek-array-signed-long-long		peek-array-unsigned-long-long
    peek-array-float			peek-array-double
    peek-array-pointer

    ;;array pokers
    (rename (pointer-set-c-char!	poke-array-char!))
    poke-array-short!			poke-array-int!
    poke-array-long!			poke-array-long-long!
    poke-array-float!			poke-array-double!
    poke-array-pointer!

    ;;conditions
    &out-of-memory
    make-out-of-memory-condition	out-of-memory-condition?
    out-of-memory-number-of-bytes	raise-out-of-memory)
  (import (nausicaa)
    (foreign memory compat)
    (foreign ffi sizeof)
    (format))



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

(define-syntax define-pointer-comparison
  (syntax-rules ()
    ((_ ?name ?func)
     (define ?name
       (case-lambda
	(()
	 #t)
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

(define (buffer-incr-used-size! buf step)
  (buffer-used-size-set! buf
			 (+ step (buffer-used-size buf))))

(define (buffer-consume-bytes! buf number-of-bytes)
  (let ((used-size	(buffer-used-size buf))
	(pointer	(memblock-pointer buf)))
    (when (> number-of-bytes used-size)
      (assertion-violation 'buffer-consume-bytes!
	(format
	    "expected buffer used size ~s <= to number of bytes to consume ~s"
	  used-size number-of-bytes)
	(list buf number-of-bytes)))
    (memmove pointer
	     (pointer-add pointer number-of-bytes)
	     number-of-bytes)
    (buffer-incr-used-size! buf (- number-of-bytes))))

(define (buffer-push-memblock! buf mb)
  (let ((copy-len	(memblock-size mb))
	(avail-len	(buffer-free-size buf)))
    (when (> copy-len avail-len)
      (assertion-violation 'buffer-push-memblock!
	(format
	    "expected source memblock with size ~s <= to the free size ~s in destination buffer"
	  copy-len avail-len)
	(list buf mb)))
    (memcpy (buffer-pointer-to-free-bytes buf)
	    (memblock-pointer mb)
	    copy-len)
    (buffer-incr-used-size! buf copy-len)))

(define (buffer-pop-memblock! mb buf)
  (let* ((src-ptr	(memblock-pointer buf))
	 (dst-ptr	(memblock-pointer mb))
	 (avail-size	(buffer-used-size buf))
	 (copy-size	(memblock-size    mb)))
    (when (> copy-size avail-size)
      (assertion-violation 'buffer-pop-memblock!
	(format
	    "expected destination memblock with size ~s <= to the used size ~s in source buffer"
	  copy-size avail-size)
	(list mb buf)))
    (memcpy dst-ptr src-ptr copy-size)
    (buffer-consume-bytes! buf copy-size)))

(define (buffer-push-bytevector! buf bv)
  (let ((copy-size	(bytevector-length bv))
	(avail-size	(buffer-free-size  buf))
	(p		(memblock-pointer  buf)))
    (when (> copy-size avail-size)
      (assertion-violation 'buffer-push-bytevector!
	(format
	    "expected source bytevector length ~s <= to the free size ~s in destination buffer"
	  copy-size avail-size)
	(list buf bv)))
    (do ((i 0 (+ 1 i)))
	((= i copy-size)
	 (buffer-incr-used-size! buf copy-size))
      (pointer-set-c-char! p i (bytevector-u8-ref bv i)))))

(define (buffer-pop-bytevector! bv buf)
  (let ((copy-size	(bytevector-length bv))
	(avail-size	(buffer-used-size  buf))
	(p		(memblock-pointer  buf)))
    (when (> copy-size avail-size)
      (assertion-violation 'buffer-pop-bytevector!
	(format
	    "expected destination bytevector length ~s <= to the free size ~s in source buffer"
	  copy-size avail-size)
	(list buf bv)))
    (do ((i 0 (+ 1 i)))
	((= i copy-size)
	 (buffer-consume-bytes! buf copy-size))
      (bytevector-u8-set! bv i (pointer-ref-c-unsigned-char p i)))))

(define (buffer-push-buffer! dst src)
  (let* ((used-size	(memblock-size src))
	 (free-size	(buffer-free-size dst))
	 (copy-size	(min free-size used-size)))
    (memcpy (buffer-pointer-to-free-bytes dst)
	    (memblock-pointer src)
	    copy-size)
    (buffer-consume-bytes!  src copy-size)
    (buffer-incr-used-size! dst copy-size)))



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
    (let ((size		(memblock-size    memblock-or-size))
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
    (let ((size		(memblock-size    buffer-or-size))
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

(define memory-buffer-pool
  (make-parameter #f
    (lambda (obj)
      (unless (or (not obj) (buffer? obj))
	(assertion-violation 'memory-buffer-pool
	  "expected #f or memory buffer as parameter value" obj))
      obj)))

(define (primitive-buffer-malloc number-of-bytes)
  (let ((buffer (memory-buffer-pool)))
    (unless buffer
      (assertion-violation 'buffer-malloc
	"attempted buffer memory allocation but no buffer was selected"))
  (if (< number-of-bytes (buffer-free-size buffer))
      (begin0
	  (buffer-pointer-to-free-bytes buffer)
	(buffer-incr-used-size! buffer number-of-bytes))
    #f)))

(define (buffer-malloc number-of-bytes)
  (or (primitive-buffer-malloc number-of-bytes)
      (raise-out-of-memory 'buffer-malloc number-of-bytes)))



;;;; reference counting

(define malloc/refcount
  (case-lambda
   ((number-of-bytes)
    (malloc/refcount number-of-bytes malloc))
   ((number-of-bytes malloc-funk)
    (let ((p (malloc-funk (+ strideof-long number-of-bytes))))
      (pointer-set-c-long! p 0 0)
      (pointer-add p strideof-long)))))

(define-syntax refcount-set!
  (syntax-rules ()
    ((_ ?pointer ?value)
     (pointer-set-c-long! ?pointer (- strideof-long) ?value))))

(define-syntax refcount-ref
  (syntax-rules ()
    ((_ ?pointer)
     (pointer-ref-c-unsigned-long ?pointer (- strideof-long)))))

(define (pointer-acquire pointer)
  (refcount-set! pointer (+ 1 (refcount-ref pointer))))

(define pointer-release
  (case-lambda
   ((pointer)
    (pointer-release pointer primitive-free))
   ((pointer free-func)
    (let ((rc (refcount-ref pointer)))
      (if (= 1 rc)
	  (primitive-free (pointer-add pointer (- strideof-long)))
	(refcount-set! pointer (- rc 1)))))))

(define pointer-dismiss
  (case-lambda
   ((pointer)
    (pointer-dismiss pointer primitive-free))
   ((pointer free-func)
    (primitive-free (pointer-add pointer (- strideof-long))))))



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



;;;; array peekers

(define (peek-array-signed-short pointer index)
  (pointer-ref-c-signed-short pointer (* index strideof-short)))
(define (peek-array-unsigned-short pointer index)
  (pointer-ref-c-unsigned-short pointer (* index strideof-short)))

(define (peek-array-signed-int pointer index)
  (pointer-ref-c-signed-int pointer (* index strideof-int)))
(define (peek-array-unsigned-int pointer index)
  (pointer-ref-c-unsigned-int pointer (* index strideof-int)))

(define (peek-array-signed-long pointer index)
  (pointer-ref-c-signed-long pointer (* index strideof-long)))
(define (peek-array-unsigned-long pointer index)
  (pointer-ref-c-unsigned-long pointer (* index strideof-long)))

(define (peek-array-signed-long-long pointer index)
  (pointer-ref-c-signed-long-long pointer (* index strideof-long-long)))
(define (peek-array-unsigned-long-long pointer index)
  (pointer-ref-c-unsigned-long-long pointer (* index strideof-long-long)))

(define (peek-array-float pointer index)
  (pointer-ref-c-float pointer (* index strideof-float)))
(define (peek-array-double pointer index)
  (pointer-ref-c-double pointer (* index strideof-float)))

(define (peek-array-pointer pointer index)
  (pointer-ref-c-pointer pointer (* index strideof-float)))



;;;; array pokers

(define (poke-array-short! pointer index value)
  (pointer-set-c-short! pointer (* index strideof-short) value))

(define (poke-array-int! pointer index value)
  (pointer-set-c-int! pointer (* index strideof-int) value))

(define (poke-array-long! pointer index value)
  (pointer-set-c-long! pointer (* index strideof-long) value))

(define (poke-array-long-long! pointer index value)
  (pointer-set-c-long-long! pointer (* index strideof-long-long) value))

(define (poke-array-float! pointer index value)
  (pointer-set-c-float! pointer (* index strideof-float) value))

(define (poke-array-double! pointer index value)
  (pointer-set-c-double! pointer (* index strideof-float) value))

(define (poke-array-pointer! pointer index value)
  (pointer-set-c-pointer! pointer (* index strideof-float) value))



;;;; conditions

(define-condition-type &out-of-memory &error
  make-out-of-memory-condition
  out-of-memory-condition?
  (number-of-bytes out-of-memory-number-of-bytes))

(define (raise-out-of-memory who number-of-bytes)
  (raise (condition (make-who-condition who)
		    (make-message-condition "out of memory")
		    (make-out-of-memory-condition number-of-bytes))))



;;;; done

)

;;; end of file
