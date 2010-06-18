;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the interpreters library
;;;Date: Wed Jul  8, 2009
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
  (interps)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing interpreters\n")


(parametrise ((check-test-name	'basic))

  (check
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(+ 1 2)))
    => 3)

  (check
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(begin (+ 1 2))))
    => 3)

  (check
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(+ 1 2))
	(o.eval '(+ 1 4)))
    => 5)

  #t)


(parametrise ((check-test-name	'variables))

  (check
      ;;Set variables and query their values.
      ;;
      (let* (((o <interp>) (make <interp> '((rnrs))))
	     (return-value (o.eval '(let ()
				      (define-variable woppa)
				      (define-variable wippa 456)
				      (set! woppa 123)
				      (list woppa wippa)))))
	(list (o.variable-ref 'woppa #f)
	      (o.variable-ref 'wippa #f)
	      return-value))
    => '(123 456 (123 456)))

  (check
      ;;Set a variable  in one EVAL, retrieve its  value in a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(let ()
		   (define-variable woppa 123)
		   #f))
	(o.eval '(begin woppa)))
    => 123)

  (check
      ;;Predefine  a variable  and retrieve  its value  in  a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.variable-set! 'woppa 123)
	(o.eval '(begin woppa)))
    => 123)

  (check
      ;;Predefine  a variable  and retrieve  its value  in  a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.variable-set! 'woppa 123)
	(o.eval 'woppa))
    => 123)

;;; --------------------------------------------------------------------
;;; functions

  (check
      ;;Set a variable  in one EVAL, retrieve its  value in a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(let ()
		   (define-variable (woppa a)
		     (cons 123 a))
		   #f))
	(o.eval '(woppa #\b)))
    => '(123 . #\b))

  (check
      ;;Predefine  a variable  and retrieve  its value  in  a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.variable-set! 'woppa (lambda (a)
				  (cons 123 a)))
	(o.eval '(woppa #\b)))
    => '(123 . #\b))

  #t)


;;;; done

(check-report)

;;; end of file
