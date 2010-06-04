;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test file for makers
;;;Date: Sat May 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (makers)
  (makers-lib)	;this is in the tests directory
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing makers\n")


(parametrise ((check-test-name	'base))

  (let ((v 2))	;variable arguments, no fixed arguments

    (define-maker doit
      list ((:alpha	1)
	    (:beta	2)
	    (:gamma	(+ 1 v))))

    (check
	(doit)
      => '(1 2 3))

    (check
    	(doit (:alpha 10))
      => '(10 2 3))

    (check
    	(doit (:beta 20))
      => '(1 20 3))

    (check
    	(doit (:gamma 30))
      => '(1 2 30))

    (check
    	(doit (:alpha	10)
	      (:beta	20))
      => '(10 20 3))

    (check
    	(doit (:alpha	10)
	      (:gamma	30))
      => '(10 2 30))

    (check
    	(doit (:gamma	30)
	      (:beta	20))
      => '(1 20 30))

    (check
    	(doit (:alpha	10)
	      (:beta	20)
	      (:gamma	30))
      => '(10 20 30))

    (check
	(let ((b 7))
	  (doit (:beta	(+ 6 (* 2 b)))
		(:alpha	(+ 2 8))))
      => '(10 20 3))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;no variable arguments, yes fixed arguments

    (define S "ciao")

    (define-maker doit
      (list (string-ref S 2) #\b)
      ((:alpha	1)
       (:beta	2)
       (:gamma	3)))

    (check
	(doit)
      => '(#\a #\b 1 2 3))

    (check
    	(doit (:alpha 10))
      => '(#\a #\b 10 2 3))

    (check
    	(doit (:beta 20))
      => '(#\a #\b 1 20 3))

    (check
    	(doit (:gamma 30))
      => '(#\a #\b 1 2 30))

    (check
    	(doit (:alpha	10)
	      (:beta	20))
      => '(#\a #\b 10 20 3))

    (check
    	(doit (:alpha	10)
	      (:gamma	30))
      => '(#\a #\b 10 2 30))

    (check
    	(doit (:gamma	30)
	      (:beta	20))
      => '(#\a #\b 1 20 30))

    (check
    	(doit (:alpha	10)
	      (:beta	20)
	      (:gamma	30))
      => '(#\a #\b 10 20 30))

    (check
	(let ((b 7))
	  (doit (:beta	(+ 6 (* 2 b)))
		(:alpha	(+ 2 8))))
      => '(#\a #\b 10 20 3))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;yes variable arguments, no fixed arguments

    (define-maker (doit a b)
      list
      ((:alpha	1)
       (:beta	2)
       (:gamma	3)))

    (check
	(doit #\a #\b)
      => '(#\a #\b 1 2 3))

    (check
    	(doit #\a #\b
	      (:alpha 10))
      => '(#\a #\b 10 2 3))

    (check
    	(doit #\a #\b
	      (:beta 20))
      => '(#\a #\b 1 20 3))

    (check
    	(doit #\a #\b
	      (:gamma 30))
      => '(#\a #\b 1 2 30))

    (check
    	(doit #\a #\b
	      (:alpha	10)
	      (:beta	20))
      => '(#\a #\b 10 20 3))

    (check
    	(doit #\a #\b
	      (:alpha	10)
	      (:gamma	30))
      => '(#\a #\b 10 2 30))

    (check
    	(doit #\a #\b
	      (:gamma	30)
	      (:beta	20))
      => '(#\a #\b 1 20 30))

    (check
    	(doit #\a #\b
	      (:alpha	10)
	      (:beta	20)
	      (:gamma	30))
      => '(#\a #\b 10 20 30))

    (check
	(let ((b 7))
	  (doit #\a #\b
		(:beta	(+ 6 (* 2 b)))
		(:alpha	(+ 2 8))))
      => '(#\a #\b 10 20 3))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;yes variable arguments, yes fixed arguments

    (define-maker (doit a b)
      (list #\a #\b)
      ((:alpha	1)
       (:beta	2)
       (:gamma	3)))

    (check
	(doit #\p #\q)
      => '(#\a #\b #\p #\q 1 2 3))

    (check
    	(doit #\p #\q
	      (:alpha 10))
      => '(#\a #\b #\p #\q 10 2 3))

    (check
    	(doit #\p #\q
	      (:beta 20))
      => '(#\a #\b #\p #\q 1 20 3))

    (check
    	(doit #\p #\q
	      (:gamma 30))
      => '(#\a #\b #\p #\q 1 2 30))

    (check
    	(doit #\p #\q
	      (:alpha	10)
	      (:beta	20))
      => '(#\a #\b #\p #\q 10 20 3))

    (check
    	(doit #\p #\q
	      (:alpha	10)
	      (:gamma	30))
      => '(#\a #\b #\p #\q 10 2 30))

    (check
    	(doit #\p #\q
	      (:gamma	30)
	      (:beta	20))
      => '(#\a #\b #\p #\q 1 20 30))

    (check
    	(doit #\p #\q
	      (:alpha	10)
	      (:beta	20)
	      (:gamma	30))
      => '(#\a #\b #\p #\q 10 20 30))

    (check
	(let ((b 7))
	  (doit #\p #\q
		(:beta	(+ 6 (* 2 b)))
		(:alpha	(+ 2 8))))
      => '(#\a #\b #\p #\q 10 20 3))

    #f)

  #t)


(parametrise ((check-test-name	'library))

;;; these tests make use of the makers from (makers-lib)

  (check
      (doit1)
    => '(1 2 3))

  (check
      (doit1 (:alpha 10))
    => '(10 2 3))

  (check
      (doit1 (:beta 20))
    => '(1 20 3))

  (check
      (doit1 (:gamma 30))
    => '(1 2 30))

  (check
      (doit1 (:alpha	10)
	     (:beta	20))
    => '(10 20 3))

  (check
      (doit1 (:alpha	10)
	     (:gamma	30))
    => '(10 2 30))

  (check
      (doit1 (:gamma	30)
	     (:beta	20))
    => '(1 20 30))

  (check
      (doit1 (:alpha	10)
	     (:beta	20)
	     (:gamma	30))
    => '(10 20 30))

  (check
      (let ((b 7))
	(doit1 (:beta	(+ 6 (* 2 b)))
	       (:alpha	(+ 2 8))))
    => '(10 20 3))

;;; --------------------------------------------------------------------

  (check
      (doit2)
    => '(#\a #\b 1 2 3))

  (check
      (doit2 (:alpha 10))
    => '(#\a #\b 10 2 3))

  (check
      (doit2 (:beta 20))
    => '(#\a #\b 1 20 3))

  (check
      (doit2 (:gamma 30))
    => '(#\a #\b 1 2 30))

  (check
      (doit2 (:alpha	10)
	     (:beta	20))
    => '(#\a #\b 10 20 3))

  (check
      (doit2 (:alpha	10)
	     (:gamma	30))
    => '(#\a #\b 10 2 30))

  (check
      (doit2 (:gamma	30)
	     (:beta	20))
    => '(#\a #\b 1 20 30))

  (check
      (doit2 (:alpha	10)
	     (:beta	20)
	     (:gamma	30))
    => '(#\a #\b 10 20 30))

  (check
      (let ((b 7))
	(doit2 (:beta	(+ 6 (* 2 b)))
	       (:alpha	(+ 2 8))))
    => '(#\a #\b 10 20 3))

;;; --------------------------------------------------------------------

  (check
      (doit3 #\a #\b)
    => '(#\a #\b 1 2 3))

  (check
      (doit3 #\a #\b
	     (:alpha 10))
    => '(#\a #\b 10 2 3))

  (check
      (doit3 #\a #\b
	     (:beta 20))
    => '(#\a #\b 1 20 3))

  (check
      (doit3 #\a #\b
	     (:gamma 30))
    => '(#\a #\b 1 2 30))

  (check
      (doit3 #\a #\b
	     (:alpha	10)
	     (:beta	20))
    => '(#\a #\b 10 20 3))

  (check
      (doit3 #\a #\b
	     (:alpha	10)
	     (:gamma	30))
    => '(#\a #\b 10 2 30))

  (check
      (doit3 #\a #\b
	     (:gamma	30)
	     (:beta	20))
    => '(#\a #\b 1 20 30))

  (check
      (doit3 #\a #\b
	     (:alpha	10)
	     (:beta	20)
	     (:gamma	30))
    => '(#\a #\b 10 20 30))

  (check
      (let ((b 7))
	(doit3 #\a #\b
	       (:beta	(+ 6 (* 2 b)))
	       (:alpha	(+ 2 8))))
    => '(#\a #\b 10 20 3))

;;; --------------------------------------------------------------------

  (check
      (doit4 #\p #\q)
    => '(#\a #\b #\p #\q 1 2 3))

  (check
      (doit4 #\p #\q
	     (:alpha 10))
    => '(#\a #\b #\p #\q 10 2 3))

  (check
      (doit4 #\p #\q
	     (:beta 20))
    => '(#\a #\b #\p #\q 1 20 3))

  (check
      (doit4 #\p #\q
	     (:gamma 30))
    => '(#\a #\b #\p #\q 1 2 30))

  (check
      (doit4 #\p #\q
	     (:alpha	10)
	     (:beta	20))
    => '(#\a #\b #\p #\q 10 20 3))

  (check
      (doit4 #\p #\q
	     (:alpha	10)
	     (:gamma	30))
    => '(#\a #\b #\p #\q 10 2 30))

  (check
      (doit4 #\p #\q
	     (:gamma	30)
	     (:beta	20))
    => '(#\a #\b #\p #\q 1 20 30))

  (check
      (doit4 #\p #\q
	     (:alpha	10)
	     (:beta	20)
	     (:gamma	30))
    => '(#\a #\b #\p #\q 10 20 30))

  (check
      (let ((b 7))
	(doit4 #\p #\q
	       (:beta	(+ 6 (* 2 b)))
	       (:alpha	(+ 2 8))))
    => '(#\a #\b #\p #\q 10 20 3))

  #t)


(parametrise ((check-test-name	'errors))

  (check
      (guard (E ((syntax-violation? E)
		 (condition-message E))
		(else
		 (write E)(newline)
		 #f))
	(eval '(let ()
		 (define-maker 123
		   list
		   ((alpha	1)
		    (beta	2)
		    (gamma	3)))
		 'bad)
	      (environment '(rnrs) '(makers))))
    => "expected identifier as maker name in maker definition")

  (check
      (guard (E ((syntax-violation? E)
		 (condition-message E))
		(else
		 (write E)(newline)
		 #f))
	(eval '(let ()
		 (define-maker (doit 123)
		   list
		   ((alpha	1)
		    (beta	2)
		    (gamma	3)))
		 'bad)
	      (environment '(rnrs) '(makers))))
    => "expected identifiers as positional argument names")

  (check
      (guard (E ((syntax-violation? E)
		 (condition-message E))
		(else
		 (write E)(newline)
		 #f))
	(eval '(let ()
		 (define-maker doit
		   list
		   ((alpha	1)
		    (beta	2)
		    (gamma	3)))
		 (doit (123 9)))
	      (environment '(rnrs) '(makers))))
    => "expected identifier as first element of maker argument clause")

  (check
      (guard (E ((syntax-violation? E)
		 (condition-message E))
		(else
		 (write E)(newline)
		 #f))
	(eval '(let ()
		 (define-maker doit
		   list
		   ((alpha	1)
		    (beta	2)
		    (gamma	3)))
		 (doit (ciao 9)))
	      (environment '(rnrs) '(makers))))
    => "unrecognised argument keyword, expected one among: alpha, beta, gamma")

  #t)


;;;; done

(check-report)

;;; end of file
