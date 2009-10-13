;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory functions for bytevectors
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
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


(import (nausicaa)
  (checks)
  (foreign memory)
  (foreign memory compensated)
  (compensations))

(check-set-mode! 'report-failed)
(display "*** testing memory bytevectors\n")


(parametrise ((check-test-name 'pointers))
  (with-compensations
    (let ((bv #vu8(0 1 2 3 4 5 6 7 8 9)))
      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated)
	   10)
	=> bv)

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated)
	   5)
	=> #vu8(0 1 2 3 4))

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated)
	   5
	   3)
	=> #vu8(3 4 5 6 7))

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated 8)
	   8)
	=> #vu8(0 1 2 3 4 5 6 7))

      (check
	  (pointer->bytevector
	   (bytevector->pointer bv malloc-block/compensated 5 2)
	   5)
	=> #vu8(2 3 4 5 6))

      #f)
    #f)
  #t)


(parametrise ((check-test-name 'memblocks))
  (with-compensations
    (let ((bv #vu8(0 1 2 3 4 5 6 7 8 9)))

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated)
	   10)
	=> bv)

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated)
	   5)
	=> #vu8(0 1 2 3 4))

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated)
	   5
	   3)
	=> #vu8(3 4 5 6 7))

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated 8)
	   8)
	=> #vu8(0 1 2 3 4 5 6 7))

      (check
	  (memblock->bytevector
	   (bytevector->memblock bv malloc-block/compensated 5 2)
	   5)
	=> #vu8(2 3 4 5 6))

      #f)
    #f)
  #t)


;;;; done

(check-report)

;;; end of file
