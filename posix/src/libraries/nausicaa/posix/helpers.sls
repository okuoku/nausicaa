;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: miscellaneous helpers
;;;Date: Sun Oct 18, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix helpers)
  (export
    define-primitive-parameter
    define-parametrised)
  (import (rnrs)
    (only (nausicaa language parameters) make-parameter))

  (define-syntax define-primitive-parameter
    ;;Define a parameter to be used to invoke a function.  Usage:
    ;;
    ;;  (define-primitive-parameter parm func)
    ;;
    ;;where  PARM is the  identifier of  the parameter  and FUNC  is the
    ;;identifier of the function.
    ;;
    (syntax-rules ()
      ((_ ?parameter-name ?primitive-name)
       (define ?parameter-name
	 (make-parameter ?primitive-name
	   (lambda (func)
	     (unless (procedure? func)
	       (assertion-violation (quote ?parameter-name)
		 (string-append "expected procedure as value for "
				?parameter-name
				" parameter")
		 func))
	     func))))))

  (define-syntax define-parametrised
    ;;Define a  parameter to  be used to  invoke a function.   Usage for
    ;;functions of fixed formals:
    ;;
    ;;   (define-parametrised NAME ARG1 ARG2 ...)
    ;;
    ;;where  NAME is the  function identifier;  the parameter  will have
    ;;identifier  "NAME-function"; the  ARG identifiers  are  the formal
    ;;arguments.
    ;;
    ;;Usage for CASE-LAMBDA functions:
    ;;
    ;;   (define-parametrised NAME ((A1 A2 ...) (B1 B2 ...) ...))
    ;;
    ;;where  NAME is the  function identifier;  the parameter  will have
    ;;identifier "NAME-function"; the function is defined as:
    ;;
    ;;   (define NAME
    ;;     (case-lambda ((A1 A2 ...) ((NAME-function) A1 A2 ...))
    ;;                   (B1 B2 ...) ((NAME-function) B1 B2 ...))
    ;;                   ...))
    ;;
    (lambda (stx)
      (syntax-case stx ()
	((_ ?name ((?arg ...) ...))
	 (let* ((name		(symbol->string (syntax->datum #'?name)))
		(parm-name	(string->symbol (string-append name "-function")))
		(prim-name	(string->symbol (string-append "primitive:" name))))
	   (with-syntax ((PARM-NAME (datum->syntax #'?name parm-name))
			 (PRIM-NAME (datum->syntax #'?name prim-name)))
	     #'(begin
		 (define-primitive-parameter PARM-NAME PRIM-NAME)
		 (define ?name
		   (case-lambda
		    ((?arg ...)
		     ((PARM-NAME) ?arg ...))
		    ...))))))
	((_ ?name ?arg ...)
	 (let* ((name		(symbol->string (syntax->datum #'?name)))
		(parm-name	(string->symbol (string-append name "-function")))
		(prim-name	(string->symbol (string-append "primitive:" name))))
	   (with-syntax ((PARM-NAME (datum->syntax #'?name parm-name))
			 (PRIM-NAME (datum->syntax #'?name prim-name)))
	     #'(begin
		 (define-primitive-parameter PARM-NAME PRIM-NAME)
		 (define (?name ?arg ...)
		   ((PARM-NAME) ?arg ...))))))
	)))

  )

;;; end of file
