;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: record type representing "struct stat"
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


(library (foreign posix stat record-types)
  (export

    <struct-stat>		<struct-stat-rtd>
    make-<struct-stat>		struct-stat->record
    <struct-stat>?

    <struct-stat>-mode
    <struct-stat>-ino
    <struct-stat>-dev
    <struct-stat>-nlink
    <struct-stat>-uid
    <struct-stat>-gid
    <struct-stat>-size
    <struct-stat>-atime
    <struct-stat>-atime_usec
    <struct-stat>-mtime
    <struct-stat>-mtime_usec
    <struct-stat>-ctime
    <struct-stat>-ctime_usec
    <struct-stat>-blocks
    <struct-stat>-blksize)
  (import (rnrs)
    (foreign posix sizeof))


(define-record-type <struct-stat>
  (fields (immutable mode)
	  (immutable ino)
	  (immutable dev)
	  (immutable nlink)
	  (immutable uid)
	  (immutable gid)
	  (immutable size)
	  (immutable atime)
	  (immutable atime_usec)
	  (immutable mtime)
	  (immutable mtime_usec)
	  (immutable ctime)
	  (immutable ctime_usec)
	  (immutable blocks)
	  (immutable blksize)))

(define <struct-stat-rtd>
  (record-type-descriptor <struct-stat>))

(define (struct-stat->record *struct-stat)
  ;;Some "struct  stat" field  may be unimplemented,  so we  default its
  ;;value to #f.
  (let-syntax ((get	(syntax-rules ()
			  ((_ ?getter)
			   (guard (exc (else #f))
			     (?getter *struct-stat))))))
    (make-struct-stat
     (get struct-stat-st_mode-ref)
     (get struct-stat-st_ino-ref)
     (get struct-stat-st_dev-ref)
     (get struct-stat-st_nlink-ref)
     (get struct-stat-st_uid-ref)
     (get struct-stat-st_gid-ref)
     (get struct-stat-st_size-ref)
     (get struct-stat-st_atime-ref)
     (get struct-stat-st_atime_usec-ref)
     (get struct-stat-st_mtime-ref)
     (get struct-stat-st_mtime_usec-ref)
     (get struct-stat-st_ctime-ref)
     (get struct-stat-st_ctime_usec-ref)
     (get struct-stat-st_blocks-ref)
     (get struct-stat-st_blksize-ref))))


;;;; done

)

;;; end of file
