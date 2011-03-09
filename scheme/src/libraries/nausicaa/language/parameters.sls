;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: parameters syntactic abstraction
;;;Date: Wed Aug 18, 2010
;;;
;;;Abstract
;;;
;;;	This  library  is  derived   from  code  in  the  Ikarus  Scheme
;;;	implementation; see below for  the original source code license.
;;;
;;;	  The reason Nausicaa  defines its own parameters is  to make it
;;;	sure that  when PARAMETRISE is used: the  validator procedure is
;;;	NOT called when the old parameter values are restored.
;;;
;;;	  Nausicaa used to rely on the underlying Scheme implementation:
;;;	the old  libraries doing that  should be still available  in the
;;;	"src/attic" directory of the distribution.
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


;;;; license of the original code from Ikarus Scheme
;;;
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.
;;;


#!r6rs
(library (nausicaa language parameters)
  (export
    make-parameter parametrise
    (rename (parametrise parameterize)
	    (parametrise parameterise)))
  (import (for (rnrs base) run expand)
    (rnrs control)
    (for (rnrs syntax-case) run expand)
    (nausicaa language threads))


(define make-parameter
  (case-lambda
   ((init-value)
    (let ()
      (define-thread-specific-location the-location init-value)
      (case-lambda
       (()
	(thread-specific-location-ref the-location))
       ((obj)
	(thread-specific-location-set! the-location obj))
       ((obj convert?)
	(thread-specific-location-set! the-location obj))
       )))
   ((init-value converter-proc)
    (let ()
      (define-thread-specific-location the-location init-value)
      (case-lambda
       (()
	(thread-specific-location-ref the-location))
       ((obj)
	(thread-specific-location-set! the-location (converter-proc obj)))
       ((obj convert?)
	(thread-specific-location-set! the-location (if convert?
							(converter-proc obj)
						      obj)))
       )))))


#| Below the ?PARM-EXPR forms  are meant to be expressions evaluating to
parameter functions.

We have to remember that, using continuations, we can jump in and out of
the  body function  any number  of times;  this means  the  out-guard of
DYNAMIC-WIND must save  the value of the parameters so  that they can be
restored by  the in-guard if the  execution flow comes back  in.  In the
following  example the  execution jumps  out and  back  into PARAMETRISE
multiple times:

(define-syntax label
  (syntax-rules ()
    ((_ ?name)
     (call/cc	;store the continuation of this form
	 (lambda (k)
	   (set! ?name k))))))

(let ((outer	#f)
      (inner	#f)
      (i	0)
      (alpha	(make-parameter 0))
      (result	'()))

  (label outer) ;store the continuation of this form
  (set! result (cons (alpha) result))
  (when inner	;jump back into parametrise after jumping out with OUTER
    (inner))

  (parametrise ((alpha 0))
    (label inner) ;store the continuation of this form
    (alpha (+ 1 (alpha)))
    (if (= 3 i)
	(list (alpha) (reverse result))
      (begin
	(set! i (+ 1 i))
	;;jump out of parametrise
	(outer)))))
=> '(4 (0 0 0 0))

The original output form in Ikarus Scheme was:

#'(let ((PARM ?parm-expr)  ...
	(VAL  ?value-expr) ...)
    (define (swap)
      (let ((TMP (PARM)) ...)
	(PARM VAL) ...
	(set! VAL TMP) ...))
    (dynamic-wind
	swap
	(lambda () ?form0 ?form ...)
	swap))

in  which: when  restoring the  values in  the out-guard,  the converted
function is  applied again  to the already  converted value; we  want to
avoid this, so below we use the CONVERT? flag.

|#
(define-syntax parametrise
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((?parm-expr ?value-expr) ...) . ?body)
       (let ((parms #'(?parm-expr ...)))
	 (with-syntax (((PARM ...) (generate-temporaries parms))
		       ((TMP  ...) (generate-temporaries parms))
		       ((VAL  ...) (generate-temporaries parms)))
	   #'(let ((PARM	?parm-expr)  ...
		   (VAL		?value-expr) ...
		   (convert?	#t)) ;convert VAL only the first time
	       (define (swap)
		 (let ((TMP (PARM)) ...)
		   (PARM VAL convert?) ...
		   (set! convert? #f)
		   (set! VAL TMP) ...))
	       (dynamic-wind
		   swap
		   (lambda () . ?body)
		   swap))
	   )))
      )))


;;;; done

)

;;; end of file
