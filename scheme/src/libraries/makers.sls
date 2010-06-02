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
    ;;Notice that  we need  to have the  helpers in a  different library
    ;;loaded for  expand, because  the functions are  used by  the newly
    ;;defined macros, not only by DEFINE-MAKER.
    (for (makers helpers) expand))


(define-syntax define-maker
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?name ?maker-sexp ?keywords-and-defaults)
       (not (or (identifier? #'?name)
		(let ((L (syntax->list #'?name)))
		  (and (pair? L)
		       (identifier? (car L))))))
       (syntax-violation 'define-maker
	 "expected identifier as maker name in maker definition"
	 (syntax->datum stx) (syntax->datum #'?name)))

      ((_ ?name ?maker-sexp ?keywords-and-defaults)
       (invalid-keywords-and-defaults? #'?keywords-and-defaults)
       (syntax-violation 'define-maker
	 "invalid format for keywords and defaults in maker definition"
	 (syntax->datum stx) (syntax->datum #'?keywords-and-defaults)))

      ((_ (?name ?var ...) . ?forms)
       (not (for-all symbol? (syntax->datum #'(?var ...))))
       (syntax-violation 'define-maker
	 "expected identifiers as positional argument names"
	 (syntax->datum stx) (syntax->datum #'(?var ...))))

      ((_ (?name ?var ...) (?maker ?arg ...) ((?keyword ?default) ...))
       #'(output-forms (?name ?var ...) (?maker ?arg ...) ((?keyword ?default) ...)))

      ((_ ?name (?maker ?arg ...) ((?keyword ?default) ...))
       #'(output-forms (?name) (?maker ?arg ...) ((?keyword ?default) ...)))

      ((_ (?name ?var ...) ?maker ((?keyword ?default) ...))
       #'(output-forms (?name ?var ...) (?maker) ((?keyword ?default) ...)))

      ((_ ?name ?maker ((?keyword ?default) ...))
       #'(output-forms (?name) (?maker) ((?keyword ?default) ...)))

      (?input-form
       (syntax-violation 'define-maker "invalid maker definition" (syntax->datum #'?input-form)))
      )))

(define-syntax output-forms
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?name ?var ...) (?maker ?arg ...) ((?keyword ?default) ...))
       (with-syntax (((VAR ...) (generate-temporaries #'(?default ...))))
	 #'(begin
	     (define VAR ?default) ...
	     (define this-context #f)
	     (define-syntax ?name
	       (lambda (use)
		 (syntax-case use ()
		   ((_ ?var ... . ?args)
		    #`(?maker ?arg ... ?var ...
			      #,@(parse-input-form-stx #'this-context (quote ?name) use #'?args
						       '((?keyword VAR) ...)))))))))))))


;;;; done

)

;;; end of file
