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
(library (nausicaa language makers)
  (export define-maker define-auxiliary-syntax define-auxiliary-syntaxes
	  mandatory optional with without)
  (import (rnrs)
    ;;Notice that  we need  to have the  helpers in a  different library
    ;;loaded for  expand, because  the functions are  used by  the newly
    ;;defined macros, not only by DEFINE-MAKER.
    (for (nausicaa language makers helpers) expand)
    (only (nausicaa language extensions)
	  define-auxiliary-syntax
	  define-auxiliary-syntaxes)
    (for (nausicaa language syntax-utilities) expand run))

  (define-auxiliary-syntaxes mandatory optional with without)


(define-syntax define-maker
  (lambda (stx)

    (define (main)
      ;;Validate  and normalise  the  input form.   Call OUTPUT-FORM  to
      ;;generate the output form.
      ;;
      (syntax-case stx ()

	((_ ?name ?maker-sexp ?keywords-defaults-options)
	 (not (or (identifier? #'?name)
		  ;;A list with an identifier in the first position.
		  (let ((L (unwrap-syntax-object #'?name)))
		    (and (pair? L)
			 (identifier? (car L))))))
	 (%synner "expected identifier as maker name in maker definition" #'?name))

	((_ ?name ?maker-sexp ?keywords-defaults-options)
	 (invalid-keywords-defaults-options? #'?keywords-defaults-options)
	 (%synner "invalid clause format in maker definition" #'?keywords-defaults-options))

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
	 (with-syntax ((((OPTION ...) ...)
			(let ((list-of-keywords (unwrap-syntax-object #'(?keyword ...))))
			  (map (lambda (keyword options-list)
				 (%parse-keyword-options keyword options-list list-of-keywords))
			    list-of-keywords
			    (unwrap-syntax-object #'((?option ...) ...))))))
	   #'(define-syntax ?name
	       (lambda (use)
		 (syntax-case use ()
		   ((_ ?use-argument ... . ?use-rest-args)
		    #`(?maker ?fixed-argument ... ?use-argument ...
			      #,@(parse-maker-input-form (quote ?name) use #'?use-rest-args
							 #'((?keyword ?default OPTION ...) ...))))))))
	 )))

    (define (%parse-keyword-options keyword options-list list-of-keywords)
      ;;Parse the list of options for a maker keyword.  Accepted options
      ;;are:
      ;;
      ;; (mandatory)		This option is mandatory.
      ;; (optional)		This option is optional.
      ;; (with ?id ...)		List of keywords required with this one.
      ;; (without ?id ...)	List of keywords mutually exclusive with
      ;;			this one.
      ;;
      ;;Return a  list of three  elements: a boolean specifying  if this
      ;;option  is mandatory;  a  list of  identifiers representing  the
      ;;keywords  with  which  this  keyword  must  appear;  a  list  of
      ;;identifiers representing the keywords with which this keyword is
      ;;mutually exclusive.
      ;;
      ;;If an  option is used  multiple times: the  last one is  the one
      ;;that matters.
      ;;
      (let loop ((options-list	options-list)
		 (mandatory?	#f)
		 (with-list	'())
		 (without-list	'()))
	(if (null? options-list)
	    (cond ((memp (lambda (id) (free-identifier=? id keyword)) with-list)
		   (%synner "maker clause keyword used in its own list of companion clauses" keyword))
		  ((memp (lambda (id) (free-identifier=? id keyword)) without-list)
		   (%synner "maker clause keyword used in its own list of mutually exclusive clauses"
			    keyword))
		  ((intersection with-list without-list)
		   => (lambda (result)
			(%synner "maker clause includes the same keywords in both companion clauses and mutually exclusive clauses"
				 (reverse result))))
		  (else
		   (for-each (lambda (k)
			       (unless (memp (lambda (id) (free-identifier=? k id)) list-of-keywords)
				 (%synner "unknown keyword in list of companion clauses" k)))
		     with-list)
		   (for-each (lambda (k)
			       (unless (memp (lambda (id) (free-identifier=? k id)) list-of-keywords)
				 (%synner "unknown keyword in list of mutually exclusive clauses" k)))
		     without-list)
		   (list mandatory? with-list without-list)))
	  (syntax-case (car options-list) (mandatory optional with without)
	    ((mandatory)
	     (loop (cdr options-list) #t with-list without-list))
	    ((optional)
	     (loop (cdr options-list) #f with-list without-list))
	    ((with ?keyword ...)
	     (all-identifiers? #'(?keyword ...))
	     (loop (cdr options-list) mandatory?
		   (unwrap-syntax-object #'(?keyword ...))
		   without-list))
	    ((without ?keyword ...)
	     (all-identifiers? #'(?keyword ...))
	     (loop (cdr options-list) mandatory? with-list
		   (unwrap-syntax-object #'(?keyword ...))))
	    (_
	     (%synner (string-append "invalid options list for keyword \""
				     (identifier->string keyword) "\"")
		      options-list))))))

    (define (intersection ell1 ell2)
      (let ((result (fold-left (lambda (knil id1)
				 (if (find (lambda (id2)
					     (free-identifier=? id1 id2))
					   ell2)
				     (cons id1 knil)
				   knil))
			       '()
			       ell1)))
	(if (null? result) #f result)))

    (define (invalid-keywords-defaults-options? keywords-defaults-options)
      (let ((keywords-defaults-options (unwrap-syntax-object keywords-defaults-options)))
	(not (and (list? keywords-defaults-options)
		  (for-all (lambda (key-default-options)
			     (and (identifier? (car key-default-options))
				  (<= 2 (length key-default-options))))
			   keywords-defaults-options)))))

    (define (%synner message subform)
      (syntax-violation 'define-maker
	message (syntax->datum stx) (and subform (syntax->datum subform))))

    (main)))


;;;; done

)

;;; end of file
