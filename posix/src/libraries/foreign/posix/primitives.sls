;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marhsaling interface for the stat functions
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


(library (foreign posix stat primitives)
  (export

    stat		fstat		lstat

    S_ISDIR		S_ISCHR		S_ISBLK
    S_ISREG		S_ISFIFO	S_ISLNK
    S_ISSOCK

    S_TYPEISMQ		S_TYPEISSEM	S_TYPEISSHM)
  (import (rnrs)
    (compensations)
    (foreign errno)
    (foreign cstrings)
    (except (foreign posix sizeof) sizeof-struct-stat)
    (prefix (only (foreign posix stat platform)
		  stat fstat lstat)
	    platform:)
    (only (foreign posix stat primitives)
	  S_ISDIR		S_ISCHR		S_ISBLK
	  S_ISREG		S_ISFIFO	S_ISLNK
	  S_ISSOCK
	  S_TYPEISMQ		S_TYPEISSEM	S_TYPEISSHM))


(define (%real-stat func funcname pathname)
  (with-compensations
    (let ((*struct-stat	(malloc-block/c sizeof-struct-stat)))
      (receive (result errno)
	  (func (string->cstring/c pathname) *struct-stat)
	(when (= -1 result)
	  (raise-errno-error funcname errno pathname))
	(struct-stat->record *struct-stat)))))

(define (stat pathname)
  (%real-stat platform:stat 'stat pathname))

(define (lstat pathname)
  (%real-stat platform:lstat 'lstat pathname))

(define (fstat fd)
  (with-compensations
    (let ((*struct-stat	(malloc-block/c sizeof-struct-stat)))
      (receive (result errno)
	  (platform:fstat fd *struct-stat)
	(when (= -1 result)
	  (raise-errno-error 'fstat errno fd))
	(struct-stat->record *struct-stat)))))


;;;; done

)

;;; end of file
