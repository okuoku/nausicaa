;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test library for class satisfaction
;;;Date: Sun Jan  9, 2011
;;;
;;;Abstract
;;;
;;;	Export binding for satisfaction  functions used in the test file
;;;	"test-class-satisfactions.sps".
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (class-satisfactions)
  (export
    has-fields-a/b/c)
  (import (nausicaa)
    (prefix (nausicaa language identifier-properties) ip.)
    (nausicaa language classes property-auxiliary-syntaxes))


(define (has-fields-a/b/c the-class-identifier)
  (let ((specs (ip.ref the-class-identifier #':field-specs)))
    (define (%synner message subform)
      (syntax-violation 'has-fields-a/b/c
	(string-append "checking class identifier "
		       (symbol->string (syntax->datum the-class-identifier))
		       ": " message)
	(syntax->datum specs) (syntax->datum subform)))
    (let loop ((specs specs)
	       (a #f) (b #f) (c #f))
      (syntax-case specs (mutable immutable)
	(()
	 (unless (and a b c)
	   (%synner "missing required field" (cond ((not a) 'a) ((not b) 'b) ((not c) 'c)))))
	(((mutable ?name ?accessor ?mutator) . ?specs)
	 (case (syntax->datum #'?name)
	   ((a)
	    (loop #'?specs #t b c))
	   ((b)
	    (loop #'?specs a #t c))
	   ((c)
	    (loop #'?specs a b #t))
	   (else
	    (loop #'?specs a b c))))
	(((immutable ?name ?accessor) . ?specs)
	 (case (syntax->datum #'?name)
	   ((a)
	    (loop #'?specs #t b c))
	   ((b)
	    (loop #'?specs a #t c))
	   ((c)
	    (loop #'?specs a b #t))
	   (else
	    (loop #'?specs a b c))))
	(?thing
	 (%synner "invalid fields specification" #'?thing))
	))))


;;;; done

)

;;; end of file
