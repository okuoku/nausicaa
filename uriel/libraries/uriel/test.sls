;;;
;;;Part of: Uriel libraries
;;;Contents: utilities for unit testing
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (uriel test)
  (export catch-exception with-result add-result)
  (import (rnrs)
    (srfi parameters))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Code.
;;; --------------------------------------------------------------------

(define result
  (make-parameter #f))

(define-syntax with-result
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (parameterize ((result '()))
       (list (begin ?form0 ?form ...)
	     (reverse (result)))))))

(define (add-result value)
  (result (cons value (result))))

(define-syntax catch-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else exc))
       ?form0 ?form ...))))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

) ;; end of library form

;;; end of file
