;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: raw memory buffers
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


(library (foreign memory membuffers)
  (export
    membuffer			membuffer*
    membuffer-used-memblock	membuffer-free-memblock!
    membuffer-incr-used-size!	membuffer-consume-bytes!
    membuffer-push-memblock!	membuffer-pop-memblock!
    membuffer-push-bytevector!	membuffer-pop-bytevector!
    membuffer-push-membuffer!)
  (import (rnrs)
    (foreign memory)
    (for (records) expand run)
    (for (foreign memory memblocks) expand))


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


;;;; done

)

;;; end of file
