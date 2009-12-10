;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling API to system inspection functions
;;;Date: Wed Dec  9, 2009
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


(library (foreign posix system primitives)
  (export
    sysconf		pathconf
    fpathconf		confstr)
  (import (rnrs)
    (receive)
    (compensations)
    (foreign memory)
    (foreign errno)
    (foreign cstrings)
    (foreign posix typedefs)
    (prefix (foreign posix system platform) platform:))


(define (sysconf param)
  (receive (result errno)
      (platform:sysconf param)
    (if (= -1 result)
	(if (= 0 errno)
	    #t
	  (raise-errno-error 'sysconf errno param))
      result)))

(define (pathconf pathname param)
  (with-compensations
    (let ((pathname-cstr (string->cstring/c pathname)))
      (receive (result errno)
	  (platform:pathconf pathname-cstr param)
	(if (= -1 result)
	    (if (= 0 errno)
		#t
	      (raise-errno-error 'pathconf errno (list pathname param)))
	  result)))))

(define (fpathconf fd param)
  (with-compensations
    (receive (result errno)
	(platform:pathconf (file-descriptor->integer) param)
      (if (= -1 result)
	  (if (= 0 errno)
	      #t
	    (raise-errno-error 'pathconf errno (list fd param)))
	result))))

(define (confstr param)
  (receive (len errno)
      (platform:confstr param pointer-null 0)
    (if (= 0 len)
	(raise-errno-error 'confstr errno param)
      (with-compensations
	(let ((cstr (malloc-block/c len)))
	  (receive (len errno)
	      (platform:confstr param cstr len)
	    (if (= 0 len)
		(raise-errno-error 'pathconf errno param)
	      (cstring->string cstr (- len 1)))))))))


;;;; done

)

;;; end of file
