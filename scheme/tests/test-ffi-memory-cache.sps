;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (ffi memory caches) allocation functions
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


#!r6rs
(import (nausicaa)
  (nausicaa checks)
  (nausicaa ffi memory)
  (nausicaa ffi memory caches)
  (nausicaa ffi memory memblocks))

(check-set-mode! 'report-failed)
(display "*** testing memory caches\n")


(parametrise ((check-test-name 'small))

  (check
      (let ((p (small-blocks-cache)))
	(begin0
	    (pointer? p)
	  (small-blocks-cache p)))
    => #t)

  (check
      (let ((ell '()))
	(do ((i 0 (+ 1 i)))
	    ((= i 9))
	  (set! ell (cons (small-blocks-cache) ell)))
	(begin0
	    (for-all pointer? ell)
	  (for-each
	      (lambda (p)
		(small-blocks-cache p))
	    ell)))
    => #t)

  (check
      (begin
	(small-blocks-cache 'purge)
	(let ((p (small-blocks-cache)))
	  (begin0
	      (pointer? p)
	    (small-blocks-cache p))))
    => #t)

  #t)


(parametrise ((check-test-name 'page))

  (check
      (let ((p (page-blocks-cache)))
	(begin0
	    (pointer? p)
	  (page-blocks-cache p)))
    => #t)

  (check
      (let ((ell '()))
	(do ((i 0 (+ 1 i)))
	    ((= i 9))
	  (set! ell (cons (page-blocks-cache) ell)))
	(begin0
	    (for-all pointer? ell)
	  (for-each
	      (lambda (p)
		(page-blocks-cache p))
	    ell)))
    => #t)

  (check
      (begin
	(page-blocks-cache 'purge)
	(let ((p (page-blocks-cache)))
	  (begin0
	      (pointer? p)
	    (page-blocks-cache p))))
    => #t)

  #t)


(parametrise ((check-test-name 'memblock))

  (check
      (let ((p (memblocks-cache 10)))
	(begin0
	    (list (<memblock>? p) (<memblock>-size p) (<memblock>-alloc-size p))
	  (memblocks-cache p)))
    => `(#t 10 ,small-blocks-size))

  (check
      (let ((ell '()))
	(do ((i 0 (+ 1 i)))
	    ((= i 9))
	  (set! ell (cons (memblocks-cache 10) ell)))
	(begin0
	    (for-all <memblock>? ell)
	  (for-each
	      (lambda (p)
		(memblocks-cache p))
	    ell)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((p (memblocks-cache 50)))
	(begin0
	    (list (<memblock>? p) (<memblock>-size p) (<memblock>-alloc-size p))
	  (memblocks-cache p)))
    => `(#t 50 ,page-blocks-size))

  (check
      (let ((ell '()))
	(do ((i 0 (+ 1 i)))
	    ((= i 9))
	  (set! ell (cons (memblocks-cache 50) ell)))
	(begin0
	    (for-all <memblock>? ell)
	  (for-each
	      (lambda (p)
		(memblocks-cache p))
	    ell)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((p (memblocks-cache 10000)))
	(begin0
	    (list (<memblock>? p) (<memblock>-size p))
	  (memblocks-cache p)))
    => '(#t 10000))

  (check
      (let ((ell '()))
	(do ((i 0 (+ 1 i)))
	    ((= i 9))
	  (set! ell (cons (memblocks-cache 10000) ell)))
	(begin0
	    (for-all <memblock>? ell)
	  (for-each
	      (lambda (p)
		(memblocks-cache p))
	    ell)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
