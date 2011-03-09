;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: memory block to bytevector conversion
;;;Date: Tue Oct 13, 2009
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


#!r6rs
(library (nausicaa ffi memory bytevectors)
  (export
    bytevector->pointer		pointer->bytevector
    bytevector->memblock	memblock->bytevector)
  (import (rnrs)
    (nausicaa ffi pointers)
    (nausicaa ffi peekers-and-pokers)
    (nausicaa ffi memory alloc)
    (nausicaa ffi memory memblocks))


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
	(pointer-c-set! uint8_t p i (bytevector-u8-ref bv (+ i offset))))))))

(define bytevector->memblock
  (case-lambda
   ((bv malloc)
    (bytevector->memblock bv malloc (bytevector-length bv) 0))
   ((bv malloc number-of-bytes)
    (bytevector->memblock bv malloc number-of-bytes 0))
   ((bv malloc number-of-bytes offset)
    (make-<memblock>
      (bytevector->pointer bv malloc number-of-bytes offset)
      number-of-bytes number-of-bytes))))

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
	(bytevector-u8-set! bv i (pointer-c-ref uint8_t pointer (+ i offset))))))))

(define memblock->bytevector
  (case-lambda
   ((blk)
    (let-syntax ((size (identifier-syntax (<memblock>-size blk))))
      (memblock->bytevector blk size 0)))
   ((blk number-of-bytes)
    (memblock->bytevector blk number-of-bytes 0))
   ((blk number-of-bytes offset)
    (let-syntax ((pointer (identifier-syntax (<memblock>-pointer blk))))
      (pointer->bytevector pointer number-of-bytes offset)))))


;;;; done

)

;;; end of file
