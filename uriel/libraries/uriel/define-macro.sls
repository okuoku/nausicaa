;; 
;; Part of: Uriel libraries for Ikarus
;; Contents: Common Lisp style macros
;; Date: Sun Nov  9, 2008
;; 
;; Abstract
;; 
;; 
;; 
;; Copyright (c) 2008 Marco Maggi
;; 
;; This  program  is free  software:  you  can redistribute  it
;; and/or modify it  under the terms of the  GNU General Public
;; License as published by the Free Software Foundation, either
;; version  3 of  the License,  or (at  your option)  any later
;; version.
;; 
;; This  program is  distributed in  the hope  that it  will be
;; useful, but  WITHOUT ANY WARRANTY; without  even the implied
;; warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;; PURPOSE.   See  the  GNU  General Public  License  for  more
;; details.
;; 
;; You should  have received a  copy of the GNU  General Public
;; License   along   with    this   program.    If   not,   see
;; <http://www.gnu.org/licenses/>.
;; 

;;page
;; ------------------------------------------------------------
;; Setup.
;; ------------------------------------------------------------

(library (uriel define-macro)
	 (export define-macro defmacro)
	 (import (rnrs))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Code.
;; ------------------------------------------------------------

(define-syntax define-macro
  (lambda (macro-definition-stx)
    (syntax-case macro-definition-stx ()
      ((_ (?name . ?args) ?form0 ?form ...)
       (syntax
	(define-macro ?name (lambda ?args ?form0 ?form ...))))
      ((_ ?name ?func)
       (syntax
	(define-syntax ?name
	  (lambda (macro-use-stx)
	    (syntax-case macro-use-stx ()
	      ((??kwd ??arg (... ...))
	       (datum->syntax
		(syntax ??kwd)
		(apply ?func (syntax->datum (syntax (??arg (... ...)))))))))))))))

(define-syntax defmacro
  (syntax-rules ()
    ((_ ?name ?args ?form ...)
     (define-macro ?name (lambda ?args ?form ...)))))

;; ------------------------------------------------------------

;;page
;; ------------------------------------------------------------
;; Done.
;; ------------------------------------------------------------

) ;; end of library form

;;; end of file
