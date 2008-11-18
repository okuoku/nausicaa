;;;
;;; Part of: Uriel libraries
;;; Contents: Scheme language extensions
;;; Date: Mon Nov  3, 2008
;;; 
;;; Abstract
;;; 
;;; 
;;; Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;; 
;;; This program is free  software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;; 
;;; This  program is  distributed  in  the hope  that  it will  be
;;; useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;;; warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more details.
;;; 
;;; You  should have  received a  copy of  the GNU  General Public
;;; License    along   with   this    program.    If    not,   see
;;; <http://www.gnu.org/licenses/>.
;;; 


;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (uriel lang)
  (export begin0)
  (import (rnrs))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Simple sintaxes.
;;; --------------------------------------------------------------------

;;;Defines  BEGIN0,  a  K  combinator  defined in  Appendix  A  ``Formal
;;;semantics'' of the R6RS document.
(define-syntax begin0
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda x
	 ?expr ...
	 (apply values x))))))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

) ;; end of library form

;;; end of file
