;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for makers
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


(library (makers helpers)
  (export syntax->list valid-keywords-and-defaults? parse-input-form-stx)
  (import (rnrs))


(define (syntax->list stx)
  (syntax-case stx ()
    (()			'())
    ((?car . ?cdr)
     (and (identifier? #'?car) (free-identifier=? #'quote #'?car))
     #'(?car . ?cdr))
    ((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
    (?atom		#'?atom)))

(define (valid-keywords-and-defaults? keywords-and-defaults)
  (for-all (lambda (key-and-default)
	     (and (identifier? (car key-and-default))
		  (null? (cddr key-and-default))))
	   keywords-and-defaults))

(define (parse-input-form-stx context who input-form-stx arguments-stx keywords-and-defaults)
  (define (%keywords-join keywords-and-defaults)
    ;;Given an alist  of keywords and default values,  join the keywords
    ;;into a string with a comma as separator; return the string.  To be
    ;;used to build error messages involving the list of keywords.
    ;;
    (let ((keys (map (lambda (p)
		       (symbol->string (car p)))
		  keywords-and-defaults)))
      (if (null? keys)
	  ""
	(call-with-values
	    (lambda ()
	      (open-string-output-port))
	  (lambda (port getter)
	    (display (car keys) port)
	    (let loop ((keys (cdr keys)))
	      (if (null? keys)
		  (getter)
		(begin
		  (display ", " port)
		  (display (car keys) port)
		  (loop (cdr keys))))))))))

  (let ((arguments-stx (syntax->list arguments-stx)))

    ;;Make sure  that ARGUMENTS-STX only holds subforms  starting with a
    ;;keyword in KEYWORDS; any order is allowed.
    (for-each (lambda (key-and-argument)
		(let ((key (car key-and-argument)))
		  (unless (identifier? key)
		    (syntax-violation who
		      "expected identifier as first element of argument subform"
		      (syntax->datum input-form-stx)
		      (syntax->datum key-and-argument)))
		  (unless (exists (lambda (key-and-default)
				    (eq? (car key-and-default) (syntax->datum key)))
				  keywords-and-defaults)
		    (syntax-violation who
		      (string-append "unrecognised argument keyword, expected one among: "
				     (%keywords-join keywords-and-defaults))
		      (syntax->datum input-form-stx)
		      (syntax->datum key)))))
      arguments-stx)

    ;;Build  and return a  list of  arguments' syntax  objects, possibly
    ;;using the given defaults.
    (map (lambda (key-and-default)
	   (let ((key (car key-and-default)))
	     (or (exists (lambda (key-and-argument)
			   (and (eq? key (syntax->datum (car key-and-argument)))
				(cadr key-and-argument)))
			 arguments-stx)
		 (datum->syntax context (cadr key-and-default)))))
      keywords-and-defaults)))


;;;; done

)

;;; end of file
