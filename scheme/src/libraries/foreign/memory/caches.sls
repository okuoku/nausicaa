;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: cached memory blocks
;;;Date: Tue Oct 13, 2009
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


(library (foreign memory caches)
  (export
    make-block-cache		make-caching-object-factory
    small-blocks-cache		page-blocks-cache
    small-blocks-size		page-blocks-size
    memblocks-cache)
  (import (rnrs)
    (records)
    (foreign ffi pointers)
    (foreign memory alloc)
    (foreign memory operations)
    (for (foreign memory memblocks) expand))


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
	((size)
	 block-size)
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
	((size)
	 (block-cache 'size))
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


;;;; done

)

;;; end of file
