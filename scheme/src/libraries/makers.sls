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
(pretty-print)
    ;;Notice that  we need to have  the helpers in  a different library,
    ;;because some functions  are used by the newly  defined macros, not
    ;;just by DEFINE-MAKER.
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
       (not (valid-keywords-and-defaults? (syntax->list #'?keywords-and-defaults)))
       (syntax-violation 'define-maker
	 "invalid format for keywords and defaults in maker definition"
	 (syntax->datum #'(?k ?name ?maker-sexp ?keywords-and-defaults))
	 (syntax->datum #'?keywords-and-defaults)))

      ((_ (?name ?var ...) (?maker ?arg ...) ?keywords-and-defaults)
       #'(define-syntax ?name
	   (lambda (use)
	     (syntax-case use ()
	       ((_ ?var ... . ?args)
		#`(?maker ?arg ... ?var ...
			  #,@(parse-input-form-stx #'?k (quote ?name) use #'?args
						   (quote ?keywords-and-defaults))))))))

      ((_ ?name (?maker ?arg ...) ?keywords-and-defaults)
       #'(define-syntax ?name
	   (lambda (use)
	     (syntax-case use ()
	       ((?k . ?args)
		#`(?maker ?arg ... #,@(parse-input-form-stx #'?k (quote ?name) use #'?args
							    (quote ?keywords-and-defaults))))))))

      ((_ (?name ?var ...) ?maker ?keywords-and-defaults)
       #'(define-syntax ?name
	   (lambda (use)
	     (syntax-case use ()
	       ((?k ?var ... . ?args)
		#`(?maker ?var ...
			  #,@(parse-input-form-stx #'?k (quote ?name) use #'?args
						   (quote ?keywords-and-defaults))))))))

      ((_ ?name ?maker ?keywords-and-defaults)
       #'(define-syntax ?name
	   (let ((keywords-and-defaults (quote ?keywords-and-defaults)))
	     (lambda (use)
	       (syntax-case use ()
		 ((?k . ?args)
		  #`(?maker #,@(parse-input-form-stx #'?k (quote ?name) use #'?args
						     keywords-and-defaults))
		  ))))))

      (?input-form
       (syntax-violation 'define-maker
	 "invalid maker definition"
	 (syntax->datum #'?input-form)))

      )))


;;;; done

)

;;; end of file
