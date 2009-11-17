;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: primitives for foreign-functions interfaces
;;;Date: Fri Oct 30, 2009
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


(library (foreign ffi primitives)
  (export
    self-shared-object
    open-shared-object		open-shared-object*
    lookup-shared-object	lookup-shared-object*
    make-c-function		make-c-function/with-errno
    pointer->c-function		pointer->c-function/with-errno
    make-c-callback		free-c-callback)
  (import (rnrs)
    (foreign ffi sizeof)
    (foreign ffi conditions)
    (foreign ffi clang-data-types)
    (prefix (only (foreign ffi platform)
		  make-c-function	make-c-function/with-errno
		  pointer->c-function	pointer->c-function/with-errno
		  make-c-callback)
	    platform:)
    (only (foreign ffi platform)
	  self-shared-object
	  open-shared-object		open-shared-object*
	  lookup-shared-object		lookup-shared-object*
	  free-c-callback
	  internal-type->implementation-type
	  implementation-data-types))


;;;; helpers

(define (%normalise-foreign-symbol foreign-symbol)
  (if (symbol? foreign-symbol)
      (symbol->string foreign-symbol)
    foreign-symbol))


(define (make-c-function lib-spec ret-type funcname arg-types)
  (platform:make-c-function lib-spec
			    (internal-type->implementation-type ret-type)
			    (%normalise-foreign-symbol funcname)
			    (map internal-type->implementation-type arg-types)))

(define (make-c-function/with-errno lib-spec ret-type funcname arg-types)
  (platform:make-c-function/with-errno lib-spec
				       (internal-type->implementation-type ret-type)
				       (%normalise-foreign-symbol funcname)
				       (map internal-type->implementation-type arg-types)))

(define (pointer->c-function ret-type address arg-types)
  (platform:pointer->c-function (internal-type->implementation-type ret-type)
				address
				(map internal-type->implementation-type arg-types)))

(define (pointer->c-function/with-errno ret-type address arg-types)
  (platform:pointer->c-function/with-errno (internal-type->implementation-type ret-type)
					   address
					   (map internal-type->implementation-type arg-types)))

(define (make-c-callback ret-type proc arg-types)
  (platform:make-c-callback (internal-type->implementation-type ret-type)
			    proc
			    (map internal-type->implementation-type arg-types)))


;;;; done

)

;;; end of file
