;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for makers
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
(library (nausicaa language makers helpers)
  (export parse-maker-input-form)
  (import (rnrs)
    (for (prefix (only (rnrs) list) rnrs.) (meta -1))
    (only (nausicaa language syntax-utilities) unwrap-syntax-object identifier->string))


(define (parse-maker-input-form who input-form-stx arguments-stx keywords-defaults-options)
  ;;Parse ARGUMENTS-STX  looking for  the auxiliary syntaxes  defined in
  ;;KEYWORDS-DEFAULTS-OPTIONS.
  ;;
  ;;WHO must be an identifier  representing the caller of this function;
  ;;to  be used  in  the  "&who" condition  object  when raising  syntax
  ;;violation errors.
  ;;
  ;;INPUT-FORM-STX must be the original full input form; to be used when
  ;;raising syntax violation errors.
  ;;
  ;;ARGUMENTS-STX must  be an  syntax object holding  null or a  list of
  ;;clauses in the form:
  ;;
  ;;	((?keyword ?value0 ?value ...) ...)
  ;;
  ;;where ?KEYWORD  is an identifier;  this list represents the  list of
  ;;clauses given in a maker invocation.
  ;;
  ;;KEYWORDS-DEFAULTS-OPTIONS  must be  a list  of lists  in  which each
  ;;sublist  represents a  clause  for  the maker;  the  format of  each
  ;;sublist must be:
  ;;
  ;;	(?keyword ?default ?boolean ?with-list ?without-list)
  ;;
  ;;where: ?KEYWORD  is the  identifier of the  clause; ?DEFAULT  is the
  ;;default value of the maker argument, to be used if the clause is not
  ;;present  in  ARGUMENTS-STX; ?BOOLEAN  specifies  if  this clause  is
  ;;mandatory  or   optional;  ?WITH-LIST  is  a   list  of  identifiers
  ;;representing  the  clauses  with  which  this  clause  must  appear;
  ;;?WITHOUT-LIST is a list of identifiers representing the clauses with
  ;;which this clause is mutually exclusive.
  ;;
  ;;It is a syntax violation if: a mandatory clause is missing, a clause
  ;;does not appear along with the  clauses in the "with" list, a clause
  ;;appears along with one of the clauses in the "without" list.
  ;;
  (define (main arguments-stx keywords-defaults-options)
    (for-each (lambda (key-and-values)
		;;Make sure that ARGUMENTS-STX has the correct format.
		(unless (pair? key-and-values)
		  (%synner "expected pair as maker clause" key-and-values))
		(unless (identifier? (car key-and-values))
		  (%synner "expected identifier as first element of maker clause" key-and-values))
		(unless (<= 2 (length key-and-values))
		  (%synner "expected list of two or more values as maker clause" key-and-values))

		;;Make  sure  that  ARGUMENTS-STX  only  holds  subforms
		;;starting with  a keyword in KEYWORDS-DEFAULTS-OPTIONS;
		;;any order is allowed.
		(let* ((keyword             (car key-and-values))
		       (key-default-options (find (lambda (key-default-options)
						    (keyword=? keyword (car key-default-options)))
					      keywords-defaults-options)))
		  (unless key-default-options
		    (%synner (string-append "unrecognised argument keyword, expected one among: "
					    (%keywords-join keywords-defaults-options))
			     keyword))

		  ;;Check  that  this clause  has  been  used along  the
		  ;;"with" clauses.
		  (for-each (lambda (with-keyword)
			      (unless (exists (lambda (key-and-values)
						(keyword=? with-keyword (car key-and-values)))
					arguments-stx)
				(%synner (string-append "maker clause \""
							(identifier->string keyword)
							"\" used without companion clause")
					 with-keyword)))
		    (list-ref key-default-options 3))

		  ;;Check that  this clause has NOT been  used along the
		  ;;"without" clauses.
		  (for-each (lambda (without-keyword)
			      (when (exists (lambda (key-and-values)
					      (keyword=? without-keyword (car key-and-values)))
				      arguments-stx)
				(%synner (string-append "maker clause \""
							(identifier->string keyword)
							"\" used with mutually exclusive clause")
					 without-keyword)))
		    (list-ref key-default-options 4))))
      arguments-stx)

    ;;Check that each clause has been used at most once.
    ;;
    ;;We put this here rather than  in the loop above because we need to
    ;;be sure that ARGUMENTS-STX has a valid format.
    (for-each (lambda (key-and-values)
		(unless (= 1 (%count (car key-and-values) arguments-stx))
		  (%synner "maker clause used multiple times" (car key-and-values))))
      arguments-stx)

    ;;Check that all the mandatory clauses are present in ARGUMENTS-STX.
    (for-each (lambda (key-default-options)
		(when (caddr key-default-options)
		  (let ((keyword (car key-default-options)))
		    (unless (exists (lambda (key-and-values)
				      (keyword=? keyword (car key-and-values)))
			      arguments-stx)
		      (%synner "missing mandatory maker clause" keyword)))))
      keywords-defaults-options)

    ;;Build  and return a  list of  arguments' syntax  objects, possibly
    ;;using the given defaults.
    (map (lambda (key-default-options)
	   (or (let ((vals (exists (lambda (key-and-values)
				     ;; (and (eq? (syntax->datum (car key-default-options))
				     ;; 	   (syntax->datum (car key-and-values)))
				     ;;      (cadr key-and-values))
				     (and (free-identifier=? (car key-default-options)
							     (car key-and-values))
					  (cdr key-and-values)))
			     arguments-stx)))
		 (and vals (if (< 1 (length vals))
			       (cons #'rnrs.list vals)
			     (car vals))))
	       ;;If not clause in the arguments: get the default.
	       (cadr key-default-options)))
      keywords-defaults-options))

  (define (%count id unwrapped-arguments-stx)
    ;;Return the count of ID in UNWRAPPED-ARGUMENTS-STX.
    ;;
    (let loop ((ell unwrapped-arguments-stx)
	       (i   0))
      (if (null? ell)
	  i
	(loop (cdr ell)
	      (if (free-identifier=? id (caar ell))
		  (+ i 1)
		i)))))

  (define (%keywords-join keywords-defaults-options)
    ;;Given  an  alist  of  keywords,  defaults and  options:  join  the
    ;;keywords  into a  string with  a  comma as  separator; return  the
    ;;string.  To be used to  build error messages involving the list of
    ;;keywords as in "must be one among: alpha, beta, gamma".
    ;;
    (let ((keys (map (lambda (p)
		       (symbol->string (syntax->datum (car p))))
		  keywords-defaults-options)))
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

  (define keyword=? free-identifier=?)
;;; (define (keyword=? id1 id2)
;;;   (eq? (syntax->datum id1) (syntax->datum id2)))

  (define (%synner message subform)
    (syntax-violation who message (syntax->datum input-form-stx) (and subform (syntax->datum subform))))

  (main (unwrap-syntax-object arguments-stx)
	(unwrap-syntax-object keywords-defaults-options)))


;;;; done

)

;;; end of file
