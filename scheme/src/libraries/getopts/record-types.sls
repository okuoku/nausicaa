;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record type definitions for getopts
;;;Date: Wed Nov 11, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (getopts record-types)
  (export
    define-option
    <option>			<option-rtd>
    make-<option>		<option>?
    <option>-brief		<option>-long
    <option>-with-arg?		<option>-description
    <option>-action)
  (import (rnrs))


(define-record-type <option>
  (fields (immutable brief)
		;A Scheme char representing a brief option.
	  (immutable long)
		;A Scheme string representing a long option, without the
		;leading "--".
	  (immutable with-arg?)
		;Boolean, true if this option requires an argument.
	  (immutable description)
		;Scheme string describing this option.
	  (immutable action)))
		;Closure to be invoked when this option is found.

(define <option-rtd>
  (record-type-descriptor <option>))

(define-syntax define-option
  (syntax-rules (brief long with-arg? description action)
    ((_ ?name
	(brief ?brief)
	(long ?long)
	(with-arg? ?with-arg)
	(description ?description)
	(action ?action))
     (define ?name
       (make-<option> ?brief ?long ?with-arg ?description ?action)))))


;;;; done

)

;;; end of file
