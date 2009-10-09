;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record extensions for (foreign memory)
;;;Date: Mon Oct  5, 2009
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


(library (foreign memory record-extensions)
  (export)
  (import (rnrs)
    (records)
    (for (foreign memory record-typedefs) expand)
    (foreign memory compat))


(define (membuffer-empty? buf)
  (with-record-fields (((pointer pointer-free) <membuffer> buf))
    (pointer=? pointer pointer-free)))

(define (membuffer-full? buf)
  (with-record-fields ((size <membuffer> buf))
    (with-virtual-fields ((used-size <membuffer> buf))
      (= size used-size))))

(define (membuffer-free-size buf)
  (with-record-fields ((size <membuffer> buf))
    (with-virtual-fields ((used-size <membuffer> buf))
      (- size used-size))))

(define (membuffer-used-size buf)
  (with-record-fields (((pointer-used pointer-free) <membuffer> buf))
    (zero? (pointer-diff pointer-free pointer-used))))

(define (membuffer-free-size/tail buf)
  ;;Return the number of bytes free  at the tail of the buffer.  This is
  ;;useful to determine if a pushed memory block will fit in it, without
  ;;shifting the used bytes to the beginning of the allocated area.
  ;;
  (with-record-fields (((pointer first-used size used-size) <membuffer> buf))
    (- size used-size (pointer-diff first-used pointer))))

(define (membuffer-pointer-to-free-bytes buf)
  (with-record-fields (((first-used used-size) <membuffer> buf))
    (pointer-add first-used used-size)))

(define-record-extension <membuffer>
  (fields (empty?		membuffer-empty?		#f)
		;true if the buffer is empty
	  (full?		membuffer-full?			#f)
		;true if the buffer is full
	  (used-size		membuffer-used-size		#f)
		;number of free bytes
	  (free-size		membuffer-free-size		#f)
		;number of free bytes
	  (free-size/tail	membuffer-free-size/tail	#f)
		;number of free bytes at the tail of the buffer
	  (free-pointer		membuffer-pointer-to-free-bytes	#f)))
		;pointer to the first free byte


(define (mempool-free-size pool)
  (with-record-fields (((pointer next size) <mempool> pool))
    (- size (pointer-diff next pointer))))

(define-record-extension <mempool>
  (fields (free-size mempool-free-size #f)))


;;;; done

)

;;; end of file
