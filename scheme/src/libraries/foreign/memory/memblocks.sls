;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record type definitions for (foreign memory ---)
;;;Date: Tue Sep 29, 2009
;;;
;;;Abstract
;;;
;;;	Export  the definitions  of  the record  types  required by  the
;;;	various  (foreign  memory  ---)   libraries.   They  are  in  an
;;;	independent library so  that they can be made  available in both
;;;	the run and expand phases.
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


(library (foreign memory memblocks)
  (export
    <memblock>			<memblock>-with-record-fields-of
    make-<memblock>		<memblock>?
    <memblock>-pointer		<memblock>-pointer-set!
    <memblock>-size		<memblock>-size-set!
    <memblock>-alloc-size	<memblock>-alloc-size-set!

    memblock-null
    memblock-shallow-clone	memblock-deep-clone
    memblock->string-hex	string-hex->memblock
    memblock-head		memblock-tail
    memblock-head?		memblock-tail?
    memblock&tail-head		memblock&head-tail
    )
  (import (nausicaa)
    (rnrs mutable-strings)
    (foreign ffi pointers)
    (only (foreign ffi peekers-and-pokers)
	  pointer-ref-c-uint8
	  pointer-set-c-uint8!)
    (only (foreign memory operations) memcpy))


(define-class <memblock>
  (nongenerative nausicaa:ffi:memory:memblocks:<memblock>)
  (protocol (lambda (make-<top>)
	      (case-lambda
	       ((pointer size)
		((make-<top>) pointer size #f))
	       ((pointer size alloc-size)
		((make-<top>) pointer size alloc-size)))))
  (fields (mutable pointer)	 ;pointer to the allocated block
	  (mutable size)	 ;official number of bytes
	  (mutable alloc-size))) ;number of allocated bytes

(define (memblock-null)
  (make-<memblock> pointer-null 0 0))

(define (memblock-shallow-clone (mb <memblock>))
  (make-<memblock> mb.pointer mb.size))

(define (memblock-deep-clone (mb <memblock>) malloc)
  (let ((ptr (malloc mb.size)))
    (memcpy ptr mb.pointer mb.size)
    (make-<memblock> ptr mb.size mb.size)))


(define (memblock->string-hex mb)
  ;;Slow but fine for debugging purposes.
  ;;
  (let* ((ptr (<memblock>-pointer mb))
	 (len (<memblock>-size    mb))
	 (str (make-string (* 2 len))))
    (do ((i 0 (+ 1 i))
	 (j 0 (+ 1 j)))
	((= i len)
	 (string-upcase str))
      (let ((hex (number->string (pointer-ref-c-uint8 ptr i) 16)))
	(if (= 1 (string-length hex))
	    (begin
	      (string-set! str j #\0)
	      (set! j (+ 1 j))
	      (string-set! str j (string-ref hex 0)))
	  (begin
	    (string-set! str j (string-ref hex 0))
	    (set! j (+ 1 j))
	    (string-set! str j (string-ref hex 1))))))))

(define (string-hex->memblock str malloc)
  ;;Slow but fine for debugging purposes.
  ;;
  (let ((len (string-length str)))
    (unless (even? len)
      (assertion-violation 'string-hex->memblock "hex string must have even length" str))
    (let* ((size  (div len 2))
	   (ptr   (malloc size)))
      (do ((i  0 (+ 2 i))
	   (i2 2 (+ 2 i2))
	   (j  0 (+ 1 j)))
	  ((= i len)
	   (make-<memblock> ptr size size))
	(pointer-set-c-uint8! ptr j (string->number (substring str i i2) 16))))))


(define (memblock-tail (block <memblock>) tail.size)
  (assert (<= tail.size block.size))
  (make-<memblock> (pointer-add block.pointer (- block.size tail.size)) tail.size #f))

(define (memblock-head (block <memblock>) head.size)
  (assert (<= head.size block.size))
  (make-<memblock> block.pointer head.size))

(define (memblock-tail? (block <memblock>) (tail <memblock>))
  (and (pointer<=? block.pointer tail.pointer)
       (pointer=? (pointer-add block.pointer block.size)
		  (pointer-add tail.pointer  tail.size))))

(define (memblock-head? (block <memblock>) (head <memblock>))
  (and (pointer=? block.pointer head.pointer)
       (<= head.size block.size)))

(define (memblock&head-tail (block <memblock>) (head <memblock>))
  (assert (pointer=? block.pointer head.pointer))
  (assert (<= head.size block.size))
  (make-<memblock> (pointer-add block.pointer head.size) (- block.size head.size)))

(define (memblock&tail-head (block <memblock>) (tail <memblock>))
  (assert (pointer<=? block.pointer tail.pointer))
  (assert (<= tail.size block.size))
  (make-<memblock> block.pointer (- block.size tail.size)))


;;;; done

)

;;; end of file
