;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling functions for glibc system inspection
;;;Date: Tue Dec 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign glibc system primitives)
  (export
    setfsent		endfsent
    getfsent		getfsspec		getfsfile
    setmntent		endmntent
    getmntent		addmntent
    )
  (import (rnrs)
    (receive)
    (compensations)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (foreign posix sizeof)
    (foreign posix typedefs)
    (prefix (foreign glibc system platform) platform:))


;;;; reading fstab

(define (setfsent)
  (let ((result (platform:setfsent)))
    (if (= 0 result)
	(error 'setfsent "error initialising fstab file iteration")
      result)))

(define (endfsent)
  (let ((result (platform:endfsent)))
    (if (= 0 result)
	(error 'endfsent "error finalising fstab file iteration")
      result)))

(define (getfsent)
  (with-compensations
    (let ((fstab* (platform:getfsent)))
      (if (pointer-null? fstab*)
	  #f
	(pointer->struct-fstab fstab*)))))

(define (getfsspec spec)
  (with-compensations
    (let ((fstab* (platform:getfsspec (string->cstring/c spec))))
      (if (pointer-null? fstab*)
	  #f
	(pointer->struct-fstab fstab*)))))

(define (getfsfile file)
  (with-compensations
    (let ((fstab* (platform:getfsfile (string->cstring/c file))))
      (if (pointer-null? fstab*)
	  #f
	(pointer->struct-fstab fstab*)))))


;;;; reading mtab

(define (setmntent file-name open-mode)
  (with-compensations
    (receive (file* errno)
	(platform:setmntent (string->cstring/c file-name)
			    (string->cstring/c open-mode))
      (if (pointer-null? file*)
	  (raise-errno-error 'setmntent errno (list file-name open-mode))
	(pointer->FILE* file*)))))

(define (endmntent stream)
  (when (= 0 (platform:endmntent (FILE*->pointer stream)))
    (error 'endmntent "error finalising mtab file iteration" stream)))

(define (getmntent stream)
  (with-compensations
    (let* ((mntent*	(malloc-block/c sizeof-mntent))
	   (buf.len	4096) ;let's try to play it safe
	   (buf.ptr	(malloc-block/c buf.len)))
      (let ((result* (platform:getmntent_r (FILE*->pointer stream) mntent* buf.ptr buf.len)))
	(if (pointer-null? result*)
	    #f
	  (pointer->struct-mntent mntent*))))))

(define (addmntent stream mntent)
  (with-compensations
    (receive (result errno)
	(platform:addmntent (FILE*->pointer stream) (struct-mntent->pointer mntent malloc-block/c))
      (if (= 0 result)
	  result
	(raise-errno-error 'addmntent errno (list stream mntent))))))


;;;; done

)

;;; end of file
