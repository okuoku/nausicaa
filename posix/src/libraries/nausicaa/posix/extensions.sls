;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: record extensions
;;;Date: Tue Dec  8, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix extensions)
  (export
    struct-flock*
    struct-timeval*)
  (import (nausicaa)
    (nausicaa posix sizeof)
    (for (nausicaa posix typedefs) expand))


(define-record-extension struct-flock*
  (parent struct-flock)
  (fields (type		(lambda (o)   (struct-flock-l_type-ref    (struct-flock->pointer o)))
			(lambda (o v) (struct-flock-l_type-set!   (struct-flock->pointer o) v)))
	  (whence	(lambda (o)   (struct-flock-l_whence-ref  (struct-flock->pointer o)))
			(lambda (o v) (struct-flock-l_whence-set! (struct-flock->pointer o) v)))
	  (start	(lambda (o)   (struct-flock-l_start-ref   (struct-flock->pointer o)))
			(lambda (o v) (struct-flock-l_start-set!  (struct-flock->pointer o) v)))
	  (len		(lambda (o)   (struct-flock-l_len-ref     (struct-flock->pointer o)))
			(lambda (o v) (struct-flock-l_len-set!    (struct-flock->pointer o) v)))
	  (pid		(lambda (o)   (struct-flock-l_pid-ref     (struct-flock->pointer o)))
			(lambda (o v) (struct-flock-l_pid-set!    (struct-flock->pointer o) v)))))

(define-record-extension struct-timeval*
  (parent struct-timeval)
  (fields (sec	(lambda (o)   (struct-timeval-tv_sec-ref   (struct-timeval->pointer o)))
		(lambda (o v) (struct-timeval-tv_sec-set!  (struct-timeval->pointer o) v)))
	  (usec	(lambda (o)   (struct-timeval-tv_usec-ref  (struct-timeval->pointer o)))
		(lambda (o v) (struct-timeval-tv_usec-set! (struct-timeval->pointer o) v)))))



;;;; done

)

;;; end of file
