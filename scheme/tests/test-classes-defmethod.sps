;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for defmethod from classes library
;;;Date: Fri Oct  1, 2010
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
  (nausicaa checks)
  (nausicaa debugging)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing classes defmethod\n")

(debugging #t)


(parametrise ((check-test-name	'defmethod))

  (let ()	;mutable fields

    (define-class <alpha>
      (fields (mutable a)
	      (mutable b))
      (methods red blue green the-this))

    (define-class <beta>
      (fields (mutable c)
	      (mutable d)))

    (defmethod <alpha> (the-this)
      this)

    (defmethod <alpha> (red)
      (list a b))

    (defmethod <alpha> (blue (o <beta>))
      (list a b o.c o.d))

    (defmethod <alpha> (green (o <beta>))
      (cons #\a (blue o)))

    (let/with-class (((p <alpha>) (make <alpha> 1 2)))
      (check
    	  (p.the-this)
	=> p))

    (check
    	(let/with-class (((p <alpha>) (make <alpha> 1 2)))
    	  (p.red))
      => '(1 2))

    (check
    	(let/with-class (((p <alpha>) (make <alpha> 1 2))
    			 ((q <beta>)  (make <beta>  3 4)))
    	  (p.blue q))
      => '(1 2 3 4))

    (check
    	(let/with-class (((p <alpha>) (make <alpha> 1 2))
    			 ((q <beta>)  (make <beta>  3 4)))
    	  (p.green q))
      => '(#\a 1 2 3 4))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;immutable fields

    (define-class <alpha>
      (fields (immutable a)
	      (immutable b))
      (methods red blue green the-this))

    (define-class <beta>
      (fields (immutable c)
	      (immutable d)))

    (defmethod <alpha> (the-this)
      this)

    (defmethod <alpha> (red)
      (list a b))

    (defmethod <alpha> (blue (o <beta>))
      (list a b o.c o.d))

    (defmethod <alpha> (green (o <beta>))
      (cons #\a (blue o)))

    (let/with-class (((p <alpha>) (make <alpha> 1 2)))
      (check
    	  (p.the-this)
	=> p))

    (check
    	(let/with-class (((p <alpha>) (make <alpha> 1 2)))
    	  (p.red))
      => '(1 2))

    (check
    	(let/with-class (((p <alpha>) (make <alpha> 1 2))
    			 ((q <beta>)  (make <beta>  3 4)))
    	  (p.blue q))
      => '(1 2 3 4))

    (check
    	(let/with-class (((p <alpha>) (make <alpha> 1 2))
    			 ((q <beta>)  (make <beta>  3 4)))
    	  (p.green q))
      => '(#\a 1 2 3 4))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
