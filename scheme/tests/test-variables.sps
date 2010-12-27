;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the variables library
;;;Date: Tue Jul  7, 2009
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


#!r6rs
(import (nausicaa)
  (nausicaa variables)
  (nausicaa language sentinel)
  (nausicaa checks)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing variables\n")



(check
    (variable? (make-variable))
  => #t)

(check
    (variable? (make-variable 123))
  => #t)

(check
    (let ((v (make-variable)))
      (sentinel? (variable-ref v)))
  => #t)

(check
    (let ((v (make-variable 123)))
      (variable-ref v))
  => 123)

(check
    (let ((v (make-variable)))
      (variable-set! v 123)
      (variable-ref v))
  => 123)

(let ()
  (define-syntax define-variable/test
    (syntax-rules ()
      ((_ ?name)
       (define-variable/test ?name sentinel))
      ((_ ?name ?value)
       (define ?name (make-variable ?value)))))

  (define-variable/test the-var 123)
  (define-syntax v (identifier-syntax
		     (_          (variable-ref  the-var))
		     ((set! _ ?e) (variable-set! the-var ?e))))

  (check v => 123)

  (check
      (begin
	(set! v 456)
	v)
    => 456)

  )

(let ()
  (define-variable v 123)

  (check v => 123)

  (check
      (begin
	(set! v 456)
	v)
    => 456)

  )

(check
    (eval '(let ()
	     (define-variable woppa 123)
	     woppa)
	  (environment '(rnrs)
		       '(variables)))
  => 123)

(check
    (eval '(let ()
	     (define-variable (woppa) 123)
	     (woppa))
	  (environment '(rnrs)
		       '(variables)))
  => 123)

(check
    (eval '(let ()
	     (define-variable (woppa a b) (list a b))
	     (woppa 1 2))
	  (environment '(rnrs)
		       '(variables)))
  => '(1 2))

(check
    (eval '(let ()
	     (define-variable woppa
	       (lambda (value)
		 value))
	     (woppa 123))
	  (environment '(rnrs)
		       '(variables)))
  => 123)

(check
    (eval (call/cc (lambda (k)
		     (define-syntax woppa
		       (syntax-rules ()
			 ((_)
			  (k 123))))
		     (woppa)))
	  (environment '(rnrs)))
  => 123)



;;;; done

(check-report)

;;; end of file
