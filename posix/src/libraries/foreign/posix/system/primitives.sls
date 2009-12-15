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
    fpathconf		confstr

    gethostname		sethostname
    getdomainname	setdomainname
    uname
    )
  (import (rnrs)
    (receive)
    (compensations)
    (foreign memory)
    (foreign errno)
    (foreign cstrings)
    (foreign posix sizeof)
    (foreign posix typedefs)
    (prefix (foreign posix system platform) platform:))


;;;; system configuration inspection

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


;;;; host names

(define (gethostname)
  (with-compensations
    (let ((len 64))
      (let loop ((name.ptr (malloc-block/c len))
		 (name.len len))
	(receive (result errno)
	    (platform:gethostname name.ptr name.len)
	  (if (= -1 result)
	      (if (= errno ENAMETOOLONG)
		  (let ((len (* 2 name.len)))
		    (loop (malloc-block/c len) len))
		(raise-errno-error 'gethostname errno))
	    (cstring->string name.ptr)))))))

(define (sethostname host-name)
  (with-compensations
    (let* ((buf.ptr (cstring->string host-name))
	   (buf.len (strlen buf.ptr)))
      (receive (result errno)
	  (platform:sethostname buf.ptr buf.len)
	(if (= -1 result)
	    (raise-errno-error 'sethostname errno host-name)
	  result)))))

(define (getdomainname)
  (with-compensations
    (let ((len 64))
      (let loop ((name.ptr (malloc-block/c len))
		 (name.len len))
	(receive (result errno)
	    (platform:getdomainname name.ptr name.len)
	  (if (= -1 result)
	      (if (= errno ENAMETOOLONG)
		  (let ((len (* 2 name.len)))
		    (loop (malloc-block/c len) len))
		(raise-errno-error 'getdomainname errno))
	    (cstring->string name.ptr)))))))

(define (setdomainname host-name)
  (with-compensations
    (let* ((buf.ptr (cstring->string host-name))
	   (buf.len (strlen buf.ptr)))
      (receive (result errno)
	  (platform:setdomainname buf.ptr buf.len)
	(if (= -1 result)
	    (raise-errno-error 'setdomainname errno host-name)
	  result)))))


;;;; platform types

(define (uname)
  (with-compensations
    (let ((utsname* (malloc-block/c sizeof-utsname)))
      (receive (result errno)
	  (platform:uname utsname*)
	(if (= -1 result)
	    (raise-errno-error 'uname errno)
	  (pointer->struct-utsname utsname*))))))



;;;; done

)

;;; end of file
