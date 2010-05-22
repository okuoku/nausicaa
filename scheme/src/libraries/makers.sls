;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper macros for constructors
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


(library (makers)
  (export define-maker)
  (import (rnrs)
    (for (makers helpers) expand))


(define-syntax define-maker
  (lambda (stx)
    (syntax-case stx ()

      ((?k ?name ?maker-sexp ?keywords-and-defaults)
       (not (or (identifier? #'?name)
		(let ((L (syntax->list #'?name)))
		  (and (pair? L)
		       (identifier? (car L))))))
       (syntax-violation 'define-maker
	 "expected identifier as maker name in maker definition"
	 (syntax->datum #'(?k ?name ?maker-sexp ?keywords-and-defaults))
	 (syntax->datum #'?name)))

      ((?k ?name ?maker-sexp ?keywords-and-defaults)
       (null? (syntax->list #'?keywords-and-defaults))
       (syntax-violation 'define-maker
	 "invalid empty list of keywords in maker definition"
	 (syntax->datum #'(?k ?name ?maker-sexp ?keywords-and-defaults))))

      ((?k ?name ?maker-sexp ?keywords-and-defaults)
       (not (valid-keywords-and-defaults? (syntax->list #'?keywords-and-defaults)))
       (syntax-violation 'define-maker
	 "invalid format for keywords and defaults in maker definition"
	 (syntax->datum #'(?k ?name ?maker-sexp ?keywords-and-defaults))
	 (syntax->datum #'?keywords-and-defaults)))

      ((_ (?name ?var0 ?var ...) (?maker ?arg ...) ?keywords-and-defaults)
       #'(define-syntax ?name
	   (lambda (use)
	     (syntax-case use ()
	       ((_ ?var0 ?var ... . ?args)
		#`(?maker ?arg ... ?var0 ?var ...
			  #,@(parse-input-form-stx (quote ?name) use #'?args
						   (quote ?keywords-and-defaults))))))))

      ((_ ?name (?maker ?arg ...) ?keywords-and-defaults)
       #'(define-syntax ?name
	   (lambda (use)
	     (syntax-case use ()
	       ((?k . ?args)
		#`(?maker ?arg ... #,@(parse-input-form-stx (quote ?name) use #'?args
							    (quote ?keywords-and-defaults))))))))

      ((_ (?name ?var0 ?var ...) ?maker ?keywords-and-defaults)
       #'(define-syntax ?name
	   (lambda (use)
	     (syntax-case use ()
	       ((?k ?var0 ?var ... . ?args)
		#`(?maker ?var0 ?var ...
			  #,@(parse-input-form-stx (quote ?name) use #'?args
						   (quote ?keywords-and-defaults))))))))

      ((_ ?name ?maker ?keywords-and-defaults)
       #'(define-syntax ?name
	   (lambda (use)
	     (syntax-case use ()
	       ((?k . ?args)
		#`(?maker #,@(parse-input-form-stx (quote ?name) use #'?args
						   (quote ?keywords-and-defaults))))))))

      (?input-form
       (syntax-violation 'define-maker
	 "invalid maker definition"
	 (syntax->datum #'?input-form)))

      )))


;;;; done

)

;;; end of file
