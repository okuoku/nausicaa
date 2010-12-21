;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for class identifier properties
;;;Date: Tue Dec 21, 2010
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


#!r6rs
(import (nausicaa)
  (rnrs eval)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing class identifier properties\n")


(parametrise ((check-test-name	'superclass-list))

  (check
      (eval '(let ()
	       (define-class <alpha>)
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<alpha> #':list-of-superclasses #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '())

  (check
      (eval '(let ()
	       (define-class <alpha>)
	       (define-class <beta>
		 (inherit <alpha>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<beta> #':list-of-superclasses #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<alpha>))

  (check
      (eval '(let ()
	       (define-class <alpha>)
	       (define-class <beta>
		 (inherit <alpha>))
	       (define-class <delta>
		 (inherit <beta>))
	       (define-class <gamma>
		 (inherit <delta>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<gamma> #':list-of-superclasses #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<delta> <beta> <alpha>))

  #t)


(parametrise ((check-test-name	'field-tags))

  (check	;no fields
      (eval '(let ()
	       (define-class <alpha>)
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<alpha> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '())

  (check	;untagged fields
      (eval '(let ()
	       (define-class <alpha>
		 (fields a))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<alpha> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '())

  (check	;single tagged field
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<alpha> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<pair>))

  (check	;single tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair> <list>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<alpha> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<pair> <list>))

  (check	;multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<alpha> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta>
		 (inherit <alpha>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<beta> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta>
		 (inherit <alpha>)
		 (fields c))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<beta> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta>
		 (inherit <alpha>)
		 (fields (c <integer>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<beta> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<integer> <vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta>
		 (inherit <alpha>)
		 (fields (c <integer>)
			 (d <complex>)))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<beta> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<complex> <integer> <vector> <number> <pair> <list>))

  (check	;inheritance, multiple tagged field, multiple tags
      (eval '(let ()
	       (define-class <alpha>
		 (fields (a <pair> <list>)
			 (b <vector> <number>)))
	       (define-class <beta>
		 (inherit <alpha>)
		 (fields (c <integer>)
			 (d <complex>)))
	       (define-label <delta>
		 (inherit <beta>))
	       (define-syntax doit
		 (lambda (stx)
		   #`(quote #,(lookup-identifier-property #'<delta> #':list-of-field-tags #f))))
	       (doit))
	    (environment '(nausicaa)
			 '(identifier-properties)
			 '(classes internal-auxiliary-syntaxes)))
    => '(<complex> <integer> <vector> <number> <pair> <list>))

  #t)


;;;; done

(check-report)

;;; end of file
