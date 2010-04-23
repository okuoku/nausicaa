;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for deferred exceptions
;;;Date: Wed Nov 19, 2008
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
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing deferred exceptions\n")

;;; --------------------------------------------------------------------

(define-syntax catch-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else exc))
       ?form0 ?form ...))))


;;;; no deferred exceptions

;;;No error.  No DEFER-EXCEPTIONS.
(check
  (with-result
   (with-deferred-exceptions-handler
       (lambda (exc)
	 (add-result -4))
     (lambda ()
       (add-result 1)
       (add-result 2)
       3)))
  => '(3 (1 2)))

;;;No error.  With DEFER-EXCEPTIONS.
(check
    (with-result
     (with-deferred-exceptions-handler
	 (lambda (exc)
	   (add-result -4))
       (lambda ()
	 (add-result 1)
	 (defer-exceptions
	   (add-result 2)
	   (add-result 3))
	 (add-result 4)
	 5)))
  => '(5 (1 2 3 4)))

;;;No error.  With nested exception handler.
(check
    (with-result
     (with-deferred-exceptions-handler
	 (lambda (exc)
	   (add-result -2))
       (lambda ()
	 (with-exception-handler
	     (lambda (exc)
	       (add-result -1))
	   (lambda ()
	     (add-result 1)
	     (defer-exceptions
	       (add-result 2))
	     (add-result 3)
	     4)))))
  => '(4 (1 2 3)))

;;;Simple raise.
(check
    (with-result
     (with-deferred-exceptions-handler
	 (lambda (exc)
	   (add-result -2))
       (lambda ()
	 (guard (exc (else exc))
	   (add-result 1)
	   (defer-exceptions
	     (add-result 2))
	   (raise 'woppa)
	   (add-result 3)
	   4))))
  => '(woppa (1 2)))


;;;; deferred exception

;;Deferred exception in the body.
(check
    (with-result
     (with-deferred-exceptions-handler
	 (lambda (exc)
	   (add-result -2)
	   (add-result exc))
       (lambda ()
	 (add-result 1)
	 (defer-exceptions
	   (add-result 2)
	   (raise 'woppa)
	   (add-result 3))
	 (add-result 4)
	 5)))
  => '(5 (1 2 4 -2 woppa)))

;;More deferred exceptions in the body.
(check
    (with-result
     (with-deferred-exceptions-handler
	 (lambda (exc)
	   (add-result exc))
       (lambda ()
	 (add-result 1)
	 (defer-exceptions
	   (add-result 2)
	   (raise 'woppa1)
	   (add-result 3))
	 (defer-exceptions
	   (add-result 4)
	   (raise 'woppa2)
	   (add-result 5))
	 (defer-exceptions
	   (add-result 6)
	   (raise 'woppa3)
	   (add-result 7))
	 (add-result 8)
	 9)))
  => '(9 (1 2 4 6 8 woppa3 woppa2 woppa1)))

;;Deferred exception and error, both in the body.
(check
    (with-result
     (guard (exc (else
		  (add-result -3)
		  exc))
       (with-deferred-exceptions-handler
	   (lambda (exc)
	     (add-result -2)
	     (add-result exc))
	 (lambda ()
	   (add-result 1)
	   (defer-exceptions
	     (add-result 2)
	     (raise 'woppa)
	     (add-result 3))
	   (add-result 4)
	   (raise 'ulla)
	   (add-result 5)
	   6))))
  => '(ulla (1 2 4 -2 woppa -3)))

;;Deferred exception in the exception handler.
(check
    (with-result
     (guard (exc (else
		  (add-result -9)
		  exc))
       (with-deferred-exceptions-handler
	   (lambda (exc)
	     (add-result -8)
	     (add-result exc))
	 (lambda ()
	   (with-exception-handler
	       (lambda (exc)
		 (add-result -4)
		 (defer-exceptions
		   (add-result -5)
		   (raise 'woppa)
		   (add-result -6))
		 (add-result -7)
		 (raise exc))
	     (lambda ()
	       (add-result 1)
	       (raise 'ulla)
	       (add-result 2)
	       3))))))
  => '(ulla (1 -4 -5 -7 -8 woppa -9)))


;;;; done

(check-report)

;;; end of file
