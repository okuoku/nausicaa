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
  (export unwrap-syntax-object invalid-keywords-and-values? parse-input-form-stx)
  (import (rnrs)
    (for (only (rnrs) quote quasiquote syntax quasisyntax) (meta -1))
    (only (syntax-utilities) unwrap-syntax-object))


(define (invalid-keywords-and-values? keywords-and-values)
  (not (for-all (lambda (key-and-value)
		  (and (identifier? (car key-and-value))
		       (null? (cddr key-and-value))))
		(unwrap-syntax-object keywords-and-values))))

(define (parse-input-form-stx who input-form-stx arguments-stx keywords-and-defaults)
  (define unwrapped-keywords-and-defaults
    (unwrap-syntax-object keywords-and-defaults))
  (define (%keywords-join keywords-and-defaults)
    ;;Given an alist  of keywords and default values,  join the keywords
    ;;into a string with a comma as separator; return the string.  To be
    ;;used to build error messages involving the list of keywords.
    ;;
    (let ((keys (map (lambda (p)
		       (symbol->string (syntax->datum (car p))))
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

  (define (%synner message subform)
    (syntax-violation who message (syntax->datum input-form-stx) (and subform (syntax->datum subform))))

  (let ((unwrapped-arguments-stx (unwrap-syntax-object arguments-stx)))

    ;;Make sure that UNWRAPPED-ARGUMENTS-STX  has the correct format and
    ;;only    holds    subforms    starting    with   a    keyword    in
    ;;KEYWORDS-AND-DEFAULTS; any order is allowed.
    (for-each (lambda (key-and-argument)
		(unless (pair? key-and-argument)
		  (%synner "expected pair as maker clause argument" key-and-argument))
		(unless (identifier? (car key-and-argument))
		  (%synner "expected identifier as first element of maker argument clause"
			   key-and-argument))
		(unless (null? (cddr key-and-argument))
		  (%synner "expected list of two values as maker clause argument" key-and-argument))
		(unless (exists (lambda (key-and-default)
				  ;; (eq? (syntax->datum (car key-and-default))
				  ;;      (syntax->datum (car key-and-argument)))
				  (free-identifier=? (car key-and-default)
						     (car key-and-argument)))
				unwrapped-keywords-and-defaults)
		  (%synner (string-append "unrecognised argument keyword, expected one among: "
					  (%keywords-join unwrapped-keywords-and-defaults))
			   (car key-and-argument))))
      unwrapped-arguments-stx)

    ;;Build  and return a  list of  arguments' syntax  objects, possibly
    ;;using the given defaults.
    (map (lambda (key-and-default)
	   (or (exists (lambda (key-and-argument)
			 ;; (and (eq? (syntax->datum (car key-and-default))
			 ;; 	   (syntax->datum (car key-and-argument)))
			 ;;      (cadr key-and-argument))
			 (and (free-identifier=? (car key-and-default)
						 (car key-and-argument))
			      (cadr key-and-argument)))
		       unwrapped-arguments-stx)
	       (cadr key-and-default)))
      unwrapped-keywords-and-defaults)))


;;;; done

)

;;; end of file
