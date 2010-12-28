;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for refcount memory allocation
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.magg-ipsu@poste.it>
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
  (nausicaa checks)
  (nausicaa ffi memory)
  (nausicaa ffi memory refcount))

(check-set-mode! 'report-failed)
(display "*** testing memory refcount\n")


(parameterize ((check-test-name 'refcount))

  (check
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (let ((p (malloc/refcount 4096)))
	   (pointer-acquire p)
	   (pointer-release p)
	   #t)))
    => '(#t (freed)))

  (check
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (let ((p (malloc/refcount 4096)))
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-release p)
	   (pointer-release p)
	   #t)))
    => '(#t (freed)))

  (check
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (let ((p (malloc/refcount 4096)))
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-acquire p)
	   (pointer-dismiss p)
	   #t)))
    => '(#t (freed)))

  (check	;dynamic-wind
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (let ((p (malloc/rc 4096)))
	   (dynamic-wind
	       (lambda () (pointer-acquire p))
	       (lambda () #t)
	       (lambda () (pointer-release p))))))
    => '(#t (freed)))

  (check	;compensations
      (with-result
       (define (logging-free pointer)
	 (add-result 'freed)
	 (platform-free pointer))
       (parameterize ((primitive-free-function logging-free))
	 (with-compensations
	   (letrec ((p (compensate
			   (begin0-let ((p (malloc/rc 4096)))
			     (pointer-acquire p))
			 (with
			  (pointer-release p)))))
	     #t))))
    => '(#t (freed)))

  #t)


;;;; done

(check-report)

;;; end of file
