;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for mempool allocation
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (ffi memory)
  (ffi memory mempool)
  (ffi memory compensated)
  (compensations))

(check-set-mode! 'report-failed)
(display "*** testing memory mempool\n")


(parametrise ((check-test-name 'mempool))

  (define size 4096)
  (define oversize (+ 10 size))

  (check
      (with-compensations
	(parametrise ((memory-pool (make-<mempool> (malloc/c size) size)))
	  (let ((p (malloc/mempool 1000)))
	    (pointer-null? p))))
    => #f)

  (check
      (with-compensations
	(parametrise ((memory-pool (make-<mempool> (malloc/c size) size)))
	  (list (pointer-null? (malloc/mempool 1000))
		(pointer-null? (malloc/mempool 1000))
		(pointer-null? (malloc/mempool 1000))
		(pointer-null? (malloc/mempool 1000))
		(primitive-malloc/mempool 1000))))
    => '(#f #f #f #f #f))

  (check
      (with-compensations
	(parametrise ((memory-pool (make-<mempool> (malloc/c size) size)))
	  (guard (exc (else
		       (list (out-of-memory-condition? exc)
			     (condition-out-of-memory/number-of-bytes exc))))
	    (malloc/mempool oversize))))
    => `(#t ,oversize))

  #t)


;;;; done

(check-report)

;;; end of file
