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
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing makers\n")


(parametrise ((check-test-name	'base))

  (let ()	;no fixed arguments

    (define-maker doit
      list ((alpha	1)
	    (beta	2)
	    (gamma	3)))

    (check
	(doit)
      => '(1 2 3))

    (check
    	(doit (alpha 10))
      => '(10 2 3))

    (check
    	(doit (beta 20))
      => '(1 20 3))

    (check
    	(doit (gamma 30))
      => '(1 2 30))

    (check
    	(doit (alpha	10)
	      (beta	20))
      => '(10 20 3))

    (check
    	(doit (alpha	10)
	      (gamma	30))
      => '(10 2 30))

    (check
    	(doit (gamma	30)
	      (beta	20))
      => '(1 20 30))

    (check
    	(doit (alpha	10)
	      (beta	20)
	      (gamma	30))
      => '(10 20 30))

    (check
	(let ((b 7))
	  (doit (beta	(+ 6 (* 2 b)))
		(alpha	(+ 2 8))))
      => '(10 20 3))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;fixed arguments

    (define S "ciao")

    (define-maker doit
      (list (string-ref S 2) #\b)
      ((alpha	1)
       (beta	2)
       (gamma	3)))

    (check
	(doit)
      => '(#\a #\b 1 2 3))

    (check
    	(doit (alpha 10))
      => '(#\a #\b 10 2 3))

    (check
    	(doit (beta 20))
      => '(#\a #\b 1 20 3))

    (check
    	(doit (gamma 30))
      => '(#\a #\b 1 2 30))

    (check
    	(doit (alpha	10)
	      (beta	20))
      => '(#\a #\b 10 20 3))

    (check
    	(doit (alpha	10)
	      (gamma	30))
      => '(#\a #\b 10 2 30))

    (check
    	(doit (gamma	30)
	      (beta	20))
      => '(#\a #\b 1 20 30))

    (check
    	(doit (alpha	10)
	      (beta	20)
	      (gamma	30))
      => '(#\a #\b 10 20 30))

    (check
	(let ((b 7))
	  (doit (beta	(+ 6 (* 2 b)))
		(alpha	(+ 2 8))))
      => '(#\a #\b 10 20 3))

    #f)

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
		 (define-maker doit list ())
		 'bad)
	      (environment '(rnrs) '(makers))))
    => "invalid empty list of keywords in maker definition")

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
    => "expected identifier as first element of argument subform")

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
