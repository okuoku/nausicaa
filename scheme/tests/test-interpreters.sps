;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the interpreters library
;;;Date: Wed Jul  8, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (checks)
  (rnrs eval)
  (sentinel))

(check-set-mode! 'report-failed)
(display "*** testing interpreters\n")



(check
    (let ((table (make-eq-hashtable))
	  (result (eval
		   '(call/cc
			(lambda (k)
			  (define-syntax define-variable
			    (lambda (stx)
			      (syntax-case stx ()
				((_ ?name)
				 (syntax (define-variable ?name sentinel)))
				((_ ?name ?value)
				 (with-syntax (((the-var)
						(generate-temporaries (syntax (?name)))))
				   (syntax
				    (define-syntax ?name
				      (identifier-syntax
				       (_
					(call/cc (lambda (kk)
						   (k (list sentinel kk (quote ?name) sentinel)))))
				       ((set! _ ?e)
					(call/cc (lambda (kk)
						   (k (list sentinel kk (quote ?name) ?e))))))))
				   )))))
 			  (define-variable woppa)
 			  (define-variable wippa)
 			  (set! woppa 123)
 			  (set! wippa 456)
 			  (list woppa wippa)))
		   (environment '(rnrs)
				'(sentinel)))))
      (if (and (list? result)
	       (sentinel? (car result)))
	  (let ((kk      (cadr result))
		(varname (caddr result))
		(value   (cadddr result)))
	    (if (sentinel? value)
		(kk (hashtable-ref table varname sentinel))
	      (begin
		(hashtable-set! table varname value)
		(kk sentinel))))
	(list (hashtable-ref table 'woppa #f)
	      (hashtable-ref table 'wippa #f)
	      result)))
  => '(123 456 (123 456)))



;;;; done

(check-report)

;;; end of file
