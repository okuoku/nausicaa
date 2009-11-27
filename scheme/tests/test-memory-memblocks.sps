;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory functions for memblocks
;;;Date: Fri Nov 27, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (compensations)
  (checks)
  (records)
  (foreign memory)
  (for (foreign memory memblocks) expand))

(check-set-mode! 'report-failed)
(display "*** testing memory blocks\n")


(parametrise ((check-test-name 'clone))

  (check
      (with-compensations
	(let* ((original	(malloc-memblock/c 256))
	       (clone		(memblock-shallow-clone original)))
	  (memcmp (<memblock>-pointer original)
		  (<memblock>-pointer clone)
		  (<memblock>-size clone))))
    => 0)

  (check
      (with-compensations
	(let* ((original	(malloc-memblock/c 256))
	       (clone		(memblock-deep-clone original malloc)))
	  (memcmp (<memblock>-pointer original)
		  (<memblock>-pointer clone)
		  (<memblock>-size clone))))
    => 0)

  #t)


(parametrise ((check-test-name 'hex))

  (check
      (with-compensations
	(let ((mb (malloc-memblock/c 16)))
	  (with-record-fields ((((ptr pointer)) <memblock> mb))
	    (do ((i 0 (+ 1 i)))
		((= i 16))
	      (pointer-set-c-uint8! ptr i i)))
	  (memblock->string-hex mb)))
    => "000102030405060708090a0b0c0d0e0f")

  (check
      (with-compensations
	(let ((mb (malloc-memblock/c 16)))
	  (with-record-fields ((((ptr pointer)) <memblock> mb))
	    (do ((i 0 (+ 1 i)))
		((= i 16))
	      (pointer-set-c-uint8! ptr i (+ 16 i))))
	  (memblock->string-hex mb)))
    => "101112131415161718191a1b1c1d1e1f")

;;; --------------------------------------------------------------------

  (let ((str "000102030405060708090a0b0c0d0e0f"))
    (check
	(with-compensations
	  (memblock->string-hex (string-hex->memblock str malloc)))
      => str))

  (let ((str "101112131415161718191a1b1c1d1e1f"))
    (check
	(with-compensations
	  (memblock->string-hex (string-hex->memblock str malloc)))
      => str))

  #t)


(parametrise ((check-test-name 'head/tail))

  (check
      (with-compensations
	(let* ((block	(malloc-memblock/c 16))
	       (head	(memblock-head block 10)))
	  (list (memblock-head? block head)
		(memblock-tail? block head))))
    => '(#t #f))

  (check
      (with-compensations
	(let* ((block	(malloc-memblock/c 16))
	       (tail	(memblock-tail block 10)))
	  (list (memblock-head? block tail)
		(memblock-tail? block tail))))
    => '(#f #t))

  (check
      (with-compensations
	(let* ((block	(malloc-memblock/c 16))
	       (head	(memblock-head block 10))
	       (tail	(memblock&head-tail block head)))
	  (list (memblock-head? block tail)
		(memblock-tail? block tail))))
    => '(#f #t))

  (check
      (with-compensations
	(let* ((block	(malloc-memblock/c 16))
	       (tail	(memblock-tail block 10))
	       (head	(memblock&tail-head block tail)))
	  (list (memblock-head? block head)
		(memblock-tail? block head))))
    => '(#t #f))

  #t)


;;;; done

(check-report)

;;; end of file
