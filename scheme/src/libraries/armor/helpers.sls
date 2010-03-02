;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions and macros for ASCII armor encoders/decoders
;;;Date: Wed Jan 27, 2010
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


(library (armor helpers)
  (export
    << >> incr! decr! define-macro while receive
    ;;define-accessor-maker
    )
  (import (rnrs)
    (language-extensions))


(define << bitwise-arithmetic-shift-left)
(define >> bitwise-arithmetic-shift-right)

(define-syntax define-macro
  (syntax-rules ()
    ((_ (?name ?arg ...) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ...)
	  (begin ?form0 ?form ...)))))))


;; (define-syntax define-accessor-maker
;;   (syntax-rules ()
;;     ((_ ?record-string)
;;      (define-syntax define-field-accessor
;;        (lambda (stx)
;; 	 (define (%field->accessor field-stx)
;; 	   (string->symbol (string-append ?record-string
;; 					  (symbol->string (syntax->datum field-stx)))))
;; 	 (define (%field->mutator field-stx)
;; 	   (string->symbol (string-append ?record-string
;; 					  (symbol->string (syntax->datum field-stx))
;; 					  "-set!")))
;; 	 (syntax-case stx ()
;; 	   ((_ ?record ?field)
;; 	    (with-syntax ((ACCESSOR	(datum->syntax #'field (%field->accessor #'?field)))
;; 			  (MUTATOR	(datum->syntax #'field (%field->mutator  #'?field))))
;; 	      #'(define-syntax ?field
;; 		  (identifier-syntax
;; 		   (?id
;; 		    (ACCESSOR ?record))
;; 		   ((set! ?id ?e)
;; 		    (MUTATOR ?record ?e))))))))))))



;;;; done

)

;;; end of file
