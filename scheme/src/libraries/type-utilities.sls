;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: utilities for value types validation
;;;Date: Wed Sep 29, 2010
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


(library (type-utilities)
  (export

    ;; type assertion definition
    define-type-assertion
    predicate
    type-description
    value-description
    )
  (import (rnrs)
    (makers)
    (prefix (syntax-utilities) synux.)
    (only (language-extensions) define-inline))


;;;; type assertion definition

(synux.define-auxiliary-syntax predicate)
(synux.define-auxiliary-syntax type-description)
(synux.define-auxiliary-syntax value-description)

(define-maker (define-type-assertion name)
  %define-type-assertion
  ((predicate		#f)
   (type-description	#f)
   (value-description	#f)))

(define-syntax %define-type-assertion
  (lambda (stx)
    (syntax-case stx ()
      ;;?NAME must  be an identifier.  ?PREDICATE can  be anything which
      ;;evaluates  to  a   predicate  function.   ?TYPE-DESCRIPTION  and
      ;;?VALUE-DESCRIPTION can be anything which evaluates to a string.
      ;;
      ((_ ?name ?predicate ?type-description ?value-description)
       (let ()
	 (define (%synner message subform)
	   (syntax-violation 'define-type-assertion message stx subform))

	 (unless (identifier? #'?name)
	   (%synner "expected identifier as assertion object name" #'?name))

	 (with-syntax ((FUNCNAME (synux.identifier-prefix 'assert- #'?name)))
	   #'(begin
	       (define the-message
		 (string-append "expected " ?type-description " as " ?value-description))
	       (define-inline (FUNCNAME who obj)
		 (if (?predicate obj)
		     #t
		   (assertion-violation who the-message obj))))
	   ))
      ))))


;;;; done

)

;;; end of file
