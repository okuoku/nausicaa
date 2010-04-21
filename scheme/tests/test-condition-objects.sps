;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (conditions)
;;;Date: Sun Nov 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (rnrs eval)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing conditions\n")


(parameterize ((check-test-name	'definition))

  (let ()
    (define-condition &alpha
      (parent &assertion)
      (fields a b))

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (list (condition-alpha/a E) (condition-alpha/b E)))
      => '(1 2))

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (alpha-condition? E))
      => #t)

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (list (error? E)
		(assertion-violation? E)))
      => '(#f #t))

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-condition &beta)

    (check
	(let ((E (make-beta-condition)))
	  (beta-condition? E))
      => #t)

    (check
	(let ((E (make-beta-condition)))
	  (error? E))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((syntax-violation? E) (condition-message E))
		(else #f))
	(eval '(define-condition &alpha
		 (parent a) (parent b))
	      (environment '(nausicaa))))
    => "PARENT clause given twice in condition type definition")

  (check
      (guard (E ((syntax-violation? E) (condition-message E))
		(else #f))
	(eval '(define-condition &alpha
		 (fields a) (fields b))
	      (environment '(nausicaa))))
    => "FIELDS clause given twice in condition type definition")

  (check
      (guard (E ((syntax-violation? E) (condition-message E))
		(else #f))
	(eval '(define-condition &alpha
		 (fields a 123 b))
	      (environment '(nausicaa))))
    => "condition type field specification must be an identifier")

  #t)


(parameterize ((check-test-name	'unimplemented))

  (check
      (guard (E ((unimplemented-condition? E)
		 (list (who-condition? E)
		       (condition-who E)))
		(else #f))
	(raise-unimplemented-error 'woppa))
    => '(#t woppa))

  #t)


(parameterize ((check-test-name	'wrong-num-args))

  (check
      (guard (E ((wrong-num-args-condition? E)
		 (list (condition-who E)
		       (condition-message E)
		       (condition-wrong-num-args/procname E)
		       (condition-wrong-num-args/expected E)
		       (condition-wrong-num-args/given E)))
		(else #f))
	(raise-wrong-num-args-error 'woppa "hey!" 'the-proc 5 10))
    => '(woppa "hey!" the-proc 5 10))

  #t)


;;;; done

(check-report)

;;; end of file
