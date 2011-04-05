;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for class identifier properties
;;;Date: Tue Dec 21, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(display "*** testing class identifier properties\n")


(parametrise ((check-test-name	'superclass-list))

  (check
      (eval '(let ()
	       (define-syntax doit
		 (lambda (stx)
		   (prop.class? (prop.struct-properties-ref #'<top>))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => #t)

  #t)


(parametrise ((check-test-name	'superclass-list))

  (check
      (eval '(let ()
	       (define-class <alpha1>)
	       (define-syntax doit
		 (lambda (stx)
		   (let ((P (prop.struct-properties-ref #'<alpha1>)))
		     #`(quote #,(prop.class-list-of-supers P)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<top>))

  (check
      (eval '(let ()
	       (define-class <alpha2>)
	       (define-class <beta2>
		 (inherit <alpha2>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-supers
			       (prop.struct-properties-ref #'<beta2>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<alpha2> <top>))

  (check
      (eval '(let ()
	       (define-class <alpha3>)
	       (define-class <beta3>
		 (inherit <alpha3>))
	       (define-class <delta3>
		 (inherit <beta3>))
	       (define-class <gamma3>
		 (inherit <delta3>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-supers
			       (prop.struct-properties-ref #'<gamma3>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<delta3> <beta3> <alpha3> <top>))

  #t)


(parametrise ((check-test-name	'field-tags))

  (check	;no fields
      (eval '(let ()
	       (define-class <alpha4>)
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<alpha4>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '())

  (check	;untagged fields
      (eval '(let ()
	       (define-class <alpha5>
		 (fields a))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<alpha5>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '())

  (check	;single tagged field
      (eval '(let ()
	       (define-class <alpha6>
		 (fields (a <pair>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<alpha6>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<pair>))

  (check	;single tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha7>
		 (fields (a <pair> <list>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<alpha7>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<pair> <list>))

  (check	;multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha8>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<alpha8>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha9>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta9>
		 (inherit <alpha9>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<beta9>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha10>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta10>
		 (inherit <alpha10>)
		 (fields c))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<beta10>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha11>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta11>
		 (inherit <alpha11>)
		 (fields (c <integer>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<beta11>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<integer> <vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha12>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta12>
		 (inherit <alpha12>)
		 (fields (c <integer>)
			 (d <complex>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.class-list-of-field-tags
			       (prop.struct-properties-ref #'<beta12>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<complex> <integer> <vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha13>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta13>
		 (inherit <alpha13>)
		 (fields (c <integer>)
			 (d <complex>)))
	       (define-label <delta13>
		 (inherit <beta13>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(prop.label-list-of-field-tags
			       (prop.struct-properties-ref #'<delta13>)))))
	       (doit))
	    (environment '(nausicaa)
			 '(for (prefix (nausicaa language classes properties) prop.) expand)))
    => '(<complex> <integer> <vector> <number> <pair> <list>))

  #t)


;;;; done

(check-report)

;;; end of file
