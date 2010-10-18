;;; -*- coding: utf-8 -*-
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


#!r6rs
(library (makers)
  (export define-maker define-auxiliary-syntax
	  mandatory with without)
  (import (rnrs)
    ;;Notice that  we need  to have the  helpers in a  different library
    ;;loaded for  expand, because  the functions are  used by  the newly
    ;;defined macros, not only by DEFINE-MAKER.
    (for (makers helpers) expand)
    (only (syntax-utilities) define-auxiliary-syntax))

  (define-auxiliary-syntax mandatory with without)


(define-syntax define-maker
  (lambda (stx)

    (define (main)
      ;;Validate  and normalise  the  input form.   Call OUTPUT-FORM  to
      ;;generate the output form.
      ;;
      (syntax-case stx ()

	((_ ?name ?maker-sexp ?keywords-and-defaults)
	 (not (or (identifier? #'?name)
		  ;;A list with an identifier in the first position.
		  (let ((L (unwrap-syntax-object #'?name)))
		    (and (pair? L)
			 (identifier? (car L))))))
	 (%synner "expected identifier as maker name in maker definition" #'?name))

	((_ ?name ?maker-sexp ?keywords-and-defaults)
	 (invalid-keywords-and-values? #'?keywords-and-defaults)
	 (%synner "invalid format for keywords and defaults in maker definition" #'?keywords-and-defaults))

	((_ (?name ?var ...) . ?forms)
	 (not (for-all symbol? (syntax->datum #'(?var ...))))
	 (%synner "expected identifiers as positional argument names" #'(?var ...)))

	((?k (?name ?var ...) (?maker ?arg ...) ((?keyword ?default ?option ...) ...))
	 (identifier? #'?maker)
	 (output-form #'(?k (?name ?var ...) (?maker ?arg ...) ((?keyword ?default ?option ...) ...))))

	((?k ?name (?maker ?arg ...) ((?keyword ?default ?option ...) ...))
	 (identifier? #'?maker)
	 (output-form #'(?k (?name) (?maker ?arg ...) ((?keyword ?default ?option ...) ...))))

	((?k (?name ?var ...) ?maker ((?keyword ?default ?option ...) ...))
	 (identifier? #'?maker)
	 (output-form #'(?k (?name ?var ...) (?maker) ((?keyword ?default ?option ...) ...))))

	((?k ?name ?maker ((?keyword ?default ?option ...) ...))
	 (identifier? #'?maker)
	 (output-form #'(?k (?name) (?maker) ((?keyword ?default ?option ...) ...))))

	(_
	 (%synner "invalid maker definition" #f))
	))

    (define (output-form stx)
      ;;Generate the output form.
      ;;
      (syntax-case stx ()
	((_ (?name ?use-argument ...) (?maker ?fixed-argument ...) ((?keyword ?default ?option ...) ...))
	 #'(define-syntax ?name
	     (lambda (use)
	       (syntax-case use ()
		 ((_ ?use-argument ... . ?use-rest-args)
		  #`(?maker ?fixed-argument ... ?use-argument ...
			    #,@(parse-input-form-stx (quote ?name) use #'?use-rest-args
						     #'((?keyword ?default ?option ...) ...)))))))
	 )))

    (define (%synner message subform)
      (syntax-violation 'define-maker
	message (syntax->datum stx) (and subform (syntax->datum subform))))

    (main)))


;;;; done

)

;;; end of file
