;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory functions for buffers
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marcomaggi@gna.org>
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


(import (nausicaa)
  (checks)
  (foreign memory membuffers)
  (for (foreign memory memblocks) expand)
  (only (foreign memory alloc)
	malloc)
  (only (foreign memory operations)
	memcmp)
  (only (foreign memory caches)
	small-blocks-cache)
  (only (foreign memory bytevectors)
	bytevector->memblock))

(cond-expand (petite (exit)) (else #f))

(check-set-mode! 'report-failed)
(display "*** testing memory buffers\n")

(define default-bv
  '#vu8(0 1 2 3 4 5 6 7 8 9
	  10 11 12 13 14 15 16 17 18 19
	  20 21 22 23 24 25 26 27 28 29
	  30 31 32 33 34 35 36 37 38 39
	  40 41 42 43 44 45 46 47 48 49
	  50 51 52 53 54 55 56 57 58 59
	  60 61 62 63 64 65 66 67 68 69
	  70 71 72 73 74 75 76 77 78 79
	  80 81 82 83 84 85 86 87 88 89
	  90 91 92 93 94 95 96 97 98 99))

(define default-blk
  (bytevector->memblock default-bv malloc))


(parametrise ((check-test-name 'bytevector))

  (check
      (let ((mb  (membuffer small-blocks-cache))
	    (src default-bv)
	    (dst (make-bytevector 100)))
	(membuffer-push-bytevector! mb src)
	(membuffer-pop-bytevector!  mb dst)
	dst)
    => default-bv)

  #t)


(parametrise ((check-test-name 'memblock))

  (check
      (let* ((len  100)
	     (mb   (membuffer small-blocks-cache))
	     (src  default-blk)
	     (dst  (make <memblock> (malloc len) len len)))
	(membuffer-push-memblock! mb src)
	(membuffer-pop-memblock!  mb dst)
	(with-record-fields* ((pointer <memblock> dst)
			      (pointer <memblock> src))
	  (memcmp dst.pointer src.pointer len)))
    => 0)

  #t)


;;;; done

(check-report)

;;; end of file
