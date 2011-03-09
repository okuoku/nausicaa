;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface to Linux fd functions
;;;Date: Fri Jan 22, 2010
;;;
;;;Abstract
;;;
;;;
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


(library (nausicaa linux fd primitives)
  (export
    pipe2
    )
  (import (rnrs)
    (nausicaa language extensions)
    (nausicaa lanaguage compensations)
    (only (nausicaa ffi memory) malloc-small/c)
    (only (nausicaa ffi cstrings) string->cstring/c)
    (only (nausicaa ffi peekers-and-pokers) array-ref-c-signed-int)
    (only (nausicaa ffi errno) raise-errno-error)
    (nausicaa posix typedefs)
    (prefix (nausicaa linux fd platform) platform.))


(define (pipe2 flags)
  (with-compensations
    (let ((p (malloc-small/c)))
      (receive (result errno)
	  (platform.pipe2 p (if (integer? flags)
				flags
			      (pipe2-flags->value flags)))
	(if (= -1 result)
	    (raise-errno-error 'pipe2 errno (list flags))
	  (values (integer->fd (array-ref-c-signed-int p 0))
		  (integer->fd (array-ref-c-signed-int p 1))))))))


;;;; done

)

;;; end of file
