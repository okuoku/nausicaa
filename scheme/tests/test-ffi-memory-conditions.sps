;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (ffi memory) condition objects
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
  (nausicaa ffi memory))

(check-set-mode! 'report-failed)
(display "*** testing memory conditions\n")

(define (failing-alloc . args)
  #f)


(parametrise ((check-test-name 'out-of-memory))

  (check
      (guard (E ((out-of-memory-condition? E)
		 (condition-message E)))
	(raise-out-of-memory 'malloc 123))
    => "out of memory")

  (check
      (guard (E ((out-of-memory-condition? E)
		 (condition-who E)))
	(raise-out-of-memory 'malloc 123))
    => 'malloc)

  (check
      (guard (E ((out-of-memory-condition? E)
		 (condition-out-of-memory/number-of-bytes E)))
	(raise-out-of-memory 'malloc 123))
    => 123)

;;; --------------------------------------------------------------------

  (check 'this
      (guard (E ((out-of-memory-condition? E)
		 (condition-out-of-memory/number-of-bytes E)))
	(parametrise ((primitive-malloc-function failing-alloc))
	  (primitive-free (malloc 4096))
	  #t))
    => 4096)

  (check
      (guard (E ((out-of-memory-condition? E)
		 (condition-out-of-memory/number-of-bytes E)))
	(parametrise ((primitive-realloc-function failing-alloc))
	  (primitive-free (realloc (malloc 4096) 8192))
	  #t))
    => 8192)

  (check
      (guard (E ((out-of-memory-condition? E)
		 (condition-out-of-memory/number-of-bytes E)))
	(parametrise ((primitive-calloc-function failing-alloc))
	  (primitive-free (calloc 4096 4))
	  #t))
    => (* 4 4096))

  #t)


(parametrise ((check-test-name 'memory-request))

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-message E)))
	(raise-memory-request 'malloc 123 #f))
    => "out of memory")

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-who E)))
	(raise-memory-request 'malloc 123 #f))
    => 'malloc)

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-memory-request/number-of-bytes E)))
	(raise-memory-request 'malloc 123 #f))
    => 123)

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-memory-request/clean? E)))
	(raise-memory-request 'malloc 123 #t))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-memory-request/number-of-bytes E)))
	(parametrise ((primitive-malloc-function failing-alloc))
	  (primitive-free (malloc 4096))
	  #t))
    => 4096)

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-memory-request/number-of-bytes E)))
	(parametrise ((primitive-realloc-function failing-alloc))
	  (primitive-free (realloc (malloc 4096) 8192))
	  #t))
    => 8192)

  (check
      (guard (E ((memory-request-condition? E)
		 (condition-memory-request/number-of-bytes E)))
	(parametrise ((primitive-calloc-function failing-alloc))
	  (primitive-free (calloc 4096 4))
	  #t))
    => (* 4 4096))

;;; --------------------------------------------------------------------

  (check
      (with-exception-handler
	  (lambda (E)
	    (if (memory-request-condition? E)
		(platform-malloc 4096)
	      (raise E)))
	(lambda ()
	  (parametrise ((primitive-malloc-function failing-alloc))
	    (primitive-free (malloc 4096))
	    #t)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
