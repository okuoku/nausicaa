;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface to time functions
;;;Date: Wed Nov  4, 2009
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


(library (foreign posix primitives)
  (export

    clock times)
  (import (rnrs)
    (prefix (foreign posix platform) platform:))


(define (clock)
  (receive (result errno)
      (platform:clock)
    (if (= -1 result)
	(raise-errno-error 'clock errno)
      result)))

(define (times)
  (with-compensations
    (let ((p (malloc-block/c (sizeof-double-array 4))))
      (receive (result errno)
	  (platform-times p)
	(if (= -1 result)
	    (raise-errno-error 'times errno)
	  (values result
		  (make-struct-tms (array-ref-c-double p 0)
				   (array-ref-c-double p 1)
				   (array-ref-c-double p 2)
				   (array-ref-c-double p 3))))))))


;;;; done

)

;;; end of file
