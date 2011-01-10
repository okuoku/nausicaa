;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for class satisfactions
;;;Date: Sun Jan  9, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (rnrs eval)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing class satisfactions\n")

(define-syntax test
  (syntax-rules ()
    ((_ . ?body)
     (check
	 (eval '(let ()
		  (begin . ?body)
		  #t)
	       (environment '(nausicaa)
			    '(for (class-satisfactions) expand)
			    '(for (prefix (nausicaa language classes properties)
					  prop.)
				  expand)))
       => #t))))


(parametrise ((check-test-name	'fields)
	      (debugging	#f))

  (test 	;satisfies has fields
   (define-class <alpha>
     (fields a b c)
     (satisfies has-fields-a/b/c)))

  (check	;wrong has fields
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-class <alpha>
		   (fields a c)
		   (satisfies has-fields-a/b/c))
		 #t)
	      (environment '(nausicaa)
			   '(for (class-satisfactions) expand))))
    => 'b)

;;; --------------------------------------------------------------------

  (check 	;satisfies has fields
      (eval '(let ()
	       (define-label <alpha>
		 (virtual-fields a b c)
		 (satisfies has-virtual-fields-a/b/c))
	       #t)
	    (environment '(nausicaa)
			 '(for (class-satisfactions) expand)))
    => #t)

  (check	;wrong has fields
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-label <alpha>
		   (virtual-fields a c)
		   (satisfies has-virtual-fields-a/b/c))
		 #t)
	      (environment '(nausicaa)
			   '(for (class-satisfactions) expand))))
    => 'b)

  #t)


(parametrise ((check-test-name	'inherit))

  (test
   (define-class <alpha>
     (inherit <top>)
     (satisfies
      (lambda (id)
	(let ((P (prop.struct-properties-ref id)))
(write (prop.class-list-of-supers P))(newline)
	  (unless (free-identifier=? #'<top> (car (prop.class-list-of-supers P)))
	    (syntax-violation #f
	      "expected top as superclass"))))
      )))

  #t)


;;;; done

(check-report)

;;; end of file
