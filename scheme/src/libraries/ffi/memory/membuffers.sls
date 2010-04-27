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


(library (ffi memory membuffers)
  (export
    <membuffer>
    make-<membuffer>		<membuffer>?
    membuffer-push-memblock!	membuffer-pop-memblock!
    membuffer-push-bytevector!	membuffer-pop-bytevector!

    &membuffer-incomplete-push
    make-membuffer-incomplete-push-condition
    membuffer-incomplete-push-condition?
    membuffer-incomplete-push/source
    membuffer-incomplete-push/source-start-offset
    membuffer-incomplete-push/source-past-offset)
  (import (nausicaa)
    (queues)
    (only (ffi pointers) pointer=? pointer-diff)
    (ffi memory)
    (ffi memory caches)
    (ffi memory memblocks))


(define-class <membuffer>
  (inherit <queue>)
  (nongenerative nausicaa:ffi:memory:membuffer:<membuffer>)
  (protocol (lambda (make-<queue>)
	      (case-lambda
	       (()
		((make-<queue>) page-blocks-cache))
	       ((cache)
		((make-<queue>) cache)))))
  (fields (immutable cache)))

(define (%enqueue-<buffer>-in-<membuffer> (mb <membuffer>))
  (let ((pointer (mb.cache)))
    (make-<buffer> pointer (mb.cache 'size) (mb.cache 'size) pointer pointer)))

(define (%dequeue-<buffer>-from-<membuffer> (mb <membuffer>))
  (let (((buf <buffer>) (queue-dequeue! mb)))
    (mb.cache buf.pointer)))


(define-class <buffer>
  (inherit <memblock>)
  ;;If the parent has a protocol, the class must have a protocol, too?!?
  (protocol (lambda (make-<memblock>)
	      (lambda (pointer size alloc-size pointer-used pointer-free)
		((make-<memblock> pointer size alloc-size)
		 pointer-used pointer-free))))
  (fields (mutable pointer-used)	;pointer to first used byte
	  (mutable pointer-free))	;pointer to first free byte
  (virtual-fields (immutable pointer	<memblock>-pointer)
		  (immutable size	<memblock>-size)
		  (immutable empty?	%buffer-empty?)
		  (immutable full?	%buffer-full?)
		  (immutable used-size	%buffer-used-size)
		  (immutable free-size	%buffer-free-size)))

(define (%buffer-empty? (buf <buffer>))
  (pointer=? buf.pointer-used buf.pointer-free))

(define (%buffer-full? (buf <buffer>))
  (= buf.size (pointer-diff buf.pointer-free buf.pointer-used)))

(define (%buffer-free-size (buf <buffer>))
  (- buf.size (pointer-diff buf.pointer-free buf.pointer-used)))

(define (%buffer-used-size (buf <buffer>))
  (pointer-diff buf.pointer-free buf.pointer-used))


(define-condition-type &membuffer-incomplete-push
  &condition
  make-membuffer-incomplete-push-condition
  membuffer-incomplete-push-condition?
  (membuffer		membuffer-incomplete-push/membuffer)
  (source		membuffer-incomplete-push/source)
  (source-start-offset	membuffer-incomplete-push/source-start-offset)
  (source-past-offset	membuffer-incomplete-push/source-past-offset))


(define membuffer-pop-bytevector!
  (case-lambda
   ((buf bv)
    (membuffer-pop-bytevector! buf bv 0 (bytevector-length bv)))
   ((buf bv bv.start)
    (membuffer-pop-bytevector! buf bv bv.start (bytevector-length bv)))
   ((buf bv bv.start bv.past)
    (%membuffer-pop! buf bv bv.start bv.past %buffer-pop-bytevector!))))

(define membuffer-push-bytevector!
  (case-lambda
   ((buf bv)
    (membuffer-push-bytevector! buf bv 0 (bytevector-length bv)))
   ((buf bv bv.start)
    (membuffer-push-bytevector! buf bv bv.start (bytevector-length bv)))
   ((buf bv bv.start bv.past)
    (%membuffer-push! buf bv bv.start bv.past %buffer-push-bytevector!))))

(define membuffer-pop-memblock!
  (case-lambda
   ((buf blk)
    (membuffer-pop-memblock! buf blk 0 (<memblock>-size blk)))
   ((buf blk blk.start)
    (membuffer-pop-memblock! buf blk blk.start (<memblock>-size blk)))
   ((buf blk blk.start blk.past)
    (%membuffer-pop! buf blk blk.start blk.past %buffer-pop-memblock!))))

(define membuffer-push-memblock!
  (case-lambda
   ((buf blk)
    (membuffer-push-memblock! buf blk 0 (<memblock>-size blk)))
   ((buf blk blk.start)
    (membuffer-push-memblock! buf blk blk.start (<memblock>-size blk)))
   ((buf blk blk.start blk.past)
    (%membuffer-push! buf blk blk.start blk.past %buffer-push-memblock!))))


(define (%membuffer-pop! (membuf <queue>) dst dst.start dst.past popper)
  (let loop ((dst.start dst.start))
    (if (= dst.start dst.past)
	dst.past
      (with-class ((membuf.front <buffer>))
	(if membuf.empty?
	    dst.start
	  (let ((start (popper membuf.front dst dst.start dst.past)))
	    (when membuf.front.empty?
	      (%dequeue-<buffer>-from-<membuffer> membuf))
	    (loop start)))))))

(define (%membuffer-push! (membuf <queue>) src src.start src.past pusher)
  (let loop ((src.start src.start))
    (if (= src.start src.past)
	src.past
      (begin
	(when (or membuf.empty? (with-class ((membuf.rear <buffer>))
				  membuf.rear.full?))
	  (with-exception-handler
	      (lambda (E)
		(raise-continuable
		 (if (out-of-memory-condition? E)
		     (condition E (make-membuffer-incomplete-push-condition membuf src
									    src.start src.past))
		   E)))
	    (lambda ()
	      (queue-enqueue! membuf (%enqueue-<buffer>-in-<membuffer> membuf)))))
	(loop (pusher membuf.rear src src.start src.past))))))


;;;; <buffer> records operations

(define %buffer-pop-bytevector!
  ;;Fill  the bytevector BV,  starting at  BV.START and  stopping before
  ;;BV.PAST, with bytes  from BUF.  Return the offset  in the bytevector
  ;;of the position  past the last filled byte.  If BV  is filled to the
  ;;end, the return value is the length of the bytevector.
  ;;
  (case-lambda

   ((buf bv)
    (%buffer-pop-bytevector! buf bv 0 (bytevector-length bv)))

   ((buf bv bv.start)
    (%buffer-pop-bytevector! buf bv bv.start (bytevector-length bv)))

   (((buf <buffer>) bv bv.start bv.past)
    (if (zero? buf.used-size)
	bv.start
      (let ((copy-len (min (- bv.past bv.start) buf.used-size)))
	(do ((i 0 (+ 1 i)))
	    ((= i copy-len)
	     (pointer-incr! buf.pointer-used copy-len)
	     (+ bv.start copy-len))
	  (bytevector-u8-set! bv (+ bv.start i)
			      (pointer-ref-c-uint8 buf.pointer-used i))))))))

(define %buffer-pop-memblock!
  ;;Fill the  <memblock> BLK, starting  at BLK.START and  sopping before
  ;;BLK.PAST,  with bytes from  BUF.  Return  the offset  in BLK  of the
  ;;position  past  the last  filled  byte.  If  the  BLK  is filled  to
  ;;BLK.PAST, the returned value is BLK.PAST.
  ;;
  (case-lambda

   ((buf blk)
    (%buffer-pop-memblock! buf blk 0 (<memblock>-size blk)))

   ((buf blk blk.start)
    (%buffer-pop-memblock! buf blk blk.start (<memblock>-size blk)))

   (((buf <buffer>) (blk <memblock>) blk.start blk.past)
    (if (zero? buf.used-size)
	blk.start
      (let ((copy-len (min (- blk.past blk.start) buf.used-size)))
	(memcpy (pointer-add blk.pointer blk.start)
		buf.pointer-used copy-len)
	(pointer-incr! buf.pointer-used copy-len)
	(+ blk.start copy-len))))))

(define %buffer-push-bytevector!
  ;;Fill the <buffer> BUF with bytes from the bytevector BV, starting at
  ;;BV.START  and stopping  before BV.PAST.   Return the  offset  in the
  ;;bytevector  of the position  past the  last filled  byte.  If  BV is
  ;;depleted to BV.PAST, the return value is BV.PAST.
  ;;
  (case-lambda

   ((buf bv)
    (%buffer-push-bytevector! buf bv 0 (bytevector-length bv)))

   ((buf bv bv.start)
    (%buffer-push-bytevector! buf bv bv.start (bytevector-length bv)))

   (((buf <buffer>) bv bv.start bv.past)
    (if (zero? buf.free-size)
	bv.start
      (let ((copy-len (min (- bv.past bv.start) buf.free-size)))
	(do ((i 0 (+ 1 i)))
	    ((= i copy-len)
	     (pointer-incr! buf.pointer-free copy-len)
	     (+ bv.start copy-len))
	  (pointer-set-c-uint8! buf.pointer-free i
				(bytevector-u8-ref bv (+ bv.start i)))))))))

(define %buffer-push-memblock!
  ;;Fill the <buffer>  BUF with bytes from the  <memblock> BLK, starting
  ;;at BLK.START and sopping before  BLK.PAST.  Return the offset in BLK
  ;;of the position past the last depleted byte.  If the BLK is depleted
  ;;to BLK.PAST, the return value is BLK.PAST.
  ;;
  (case-lambda

   ((buf blk)
    (%buffer-push-memblock! buf blk 0 (<memblock>-size blk)))

   ((buf blk blk.start)
    (%buffer-push-memblock! buf blk blk.start (<memblock>-size blk)))

   (((buf <buffer>) (blk <memblock>) blk.start blk.past)
    (if (zero? buf.free-size)
	blk.start
      (let ((copy-len (min (- blk.past blk.start) buf.free-size)))
	(memcpy buf.pointer-free (pointer-add blk.pointer blk.start) copy-len)
	(pointer-incr! buf.pointer-free copy-len)
	(+ blk.start copy-len))))))


;;;; done

)

;;; end of file
