;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: common stuff for the nausicaa language
;;;Date: Thu Jun 18, 2009
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa common)
  (export finite? infinite? nan? non-negative? non-positive? =
	  symbol*->string symbol->string/maybe)
  (import (rename (rnrs)
		  (finite?	rnrs:finite?)
		  (infinite?	rnrs:infinite?)
		  (nan?		rnrs:nan?)
		  (=		rnrs:=)))


;;;; extensions of numeric functions

(define (finite? num)
  (if (complex? num)
      (and (rnrs:finite? (real-part num))
	   (rnrs:finite? (imag-part num)))
    (rnrs:finite? num)))

(define-syntax cplx-or-pred
  (syntax-rules ()
    ((_ ?pred ?rnrs-pred)
     (define (?pred num)
       (if (complex? num)
	   (or (?rnrs-pred (real-part num))
	       (?rnrs-pred (imag-part num)))
	 (?rnrs-pred num))))))

(cplx-or-pred infinite?	rnrs:infinite?)
(cplx-or-pred nan?	rnrs:nan?)

(define (non-negative? n)
  (or (positive? n) (zero? n)))

(define (non-positive? n)
  (or (negative? n) (zero? n)))


(define =
  (case-lambda
   (()
    #t)
   ((obj)
    (rnrs:= obj obj))
   ((obj . objs)
    (apply rnrs:= obj objs))))


;;;; miscellaneous definitions

(define (symbol*->string obj)
  (if (symbol? obj)
      (symbol->string obj)
    obj))

(define (symbol->string/maybe thing)
  (cond ((symbol? thing) (symbol->string thing))
	((string? thing) thing)
	(else
	 (assertion-violation 'symbol->string/maybe
	   "expected symbol or string" thing))))


;;;; done

)

;;; end of file
