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

    )
  (import (rnrs)
    (receive)
    (compensations)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (foreign posix typedefs)
    (prefix (foreign glibc system platform) platform:))


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


;;;; done

)

;;; end of file
