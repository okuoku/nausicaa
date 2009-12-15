;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface for users functions
;;;Date: Wed Nov  4, 2009
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


(library (foreign posix users primitives)
  (export
    getuid		getgid
    geteuid		getegid

    setuid		seteuid
    setgid		setegid

    getgroups		setgroups
    initgroups		getgrouplist

    getlogin		cuserid

    getpwuid		getpwnam
    getgrgid		getgrnam
    fgetpwent		fgetgrent)
  (import (rnrs)
    (receive)
    (compensations)
    (foreign memory)
    (foreign errno)
    (foreign cstrings)
    (foreign posix sizeof)
    (foreign posix typedefs)
    (prefix (foreign posix users platform) platform:))


;;;; accessors

(define (getuid)
  (integer->uid (platform:getuid)))

(define (getgid)
  (integer->gid (platform:getgid)))

(define (geteuid)
  (integer->uid (platform:geteuid)))

(define (getegid)
  (integer->gid (platform:getegid)))

(define (getgroups)
  (receive (group-count errno)
      (platform:getgroups 0 pointer-null)
    (if (= 0 group-count)
	'()
      (with-compensations
	(let ((groups* (malloc-block/c (sizeof-gid_t-array group-count))))
	  (receive (result errno)
	      (platform:getgroups group-count groups*)
	    (if (= -1 result)
		(raise-errno-error 'getgroups errno)
	      (let loop ((i         0)
			 (group-ids '()))
		(if (= i group-count)
		    group-ids
		  (loop (+ 1 i) (cons
				 (integer->gid
				  (pointer-ref-c-gid_t (pointer-add groups* (* i strideof-gid_t)) i))
				 group-ids)))))))))))

(define (getgrouplist user-name gid)
  (with-compensations
    (let ((user-name-cstr	(string->cstring/c user-name))
	  (gid-int		(gid->integer gid))
	  (group-count*		(malloc-small/c))
	  (groups*		(malloc-small/c)))
      (pointer-set-c-signed-int! group-count* 0 1)
      (receive (group-count errno)
	  (platform:getgrouplist user-name-cstr gid-int groups* group-count*)
	(let ((group-count (pointer-ref-c-signed-int group-count* 0)))
	  (if (= 0 group-count)
	      (list gid)
	    (let ((groups* (malloc-block/c (sizeof-gid_t-array group-count))))
	      (receive (result errno)
		  (platform:getgrouplist user-name-cstr gid-int groups* group-count*)
		(if (= -1 result)
		    (raise-errno-error 'getgroups errno (list user-name gid))
		  (let loop ((i         0)
			     (group-ids '()))
		    (if (= i group-count)
			group-ids
		      (loop (+ 1 i) (cons
				     (integer->gid
				      (pointer-ref-c-gid_t (pointer-add groups* (* i strideof-gid_t)) i))
				     group-ids)))))))))))))


;;;; mutators

(define (setuid uid)
  (receive (result errno)
      (platform:setuid (uid->integer uid))
    (if (= -1 result)
	(raise-errno-error 'setuid errno uid)
      result)))

(define (seteuid uid)
  (receive (result errno)
      (platform:seteuid (uid->integer uid))
    (if (= -1 result)
	(raise-errno-error 'seteuid errno uid)
      result)))

(define (setgid gid)
  (receive (result errno)
      (platform:setgid (gid->integer gid))
    (if (= -1 result)
	(raise-errno-error 'setgid errno gid)
      result)))

(define (setegid gid)
  (receive (result errno)
      (platform:setegid (gid->integer gid))
    (if (= -1 result)
	(raise-errno-error 'setegid errno gid)
      result)))

(define (setgroups gid-list)
  (with-compensations
    (let* ((group-count	(length gid-list))
	   (groups*	(malloc-block/c (sizeof-gid_t-array group-count))))
      (do ((i 0 (+ 1 i))
	   (ell gid-list (cdr gid-list)))
	  ((= i group-count))
	(pointer-set-c-gid_t! (pointer-add groups* (* i strideof-gid_t)) i
			      (gid->integer (car ell))))
      (receive (result errno)
	  (platform:setgroups group-count groups*)
	(if (= -1 result)
	    (raise-errno-error 'setgroups errno gid-list)
	  result)))))

(define (initgroups user-name gid)
  (with-compensations
    (let ((user-name-cstr (string->cstring/c user-name)))
      (receive (result errno)
	  (platform:initgroups user-name-cstr (gid->integer gid))
	(if (= -1 result)
	    (raise-errno-error 'initgroups errno (list user-name gid))
	  result)))))


;;;; login names

(define (getlogin)
  (cstring->string (platform:getlogin)))

(define (cuserid)
  (with-compensations
    (let ((cstr (malloc-block/c L_cuserid)))
      (platform:cuserid cstr)
      (cstring->string cstr))))


;;;; users database

(define (getpwuid uid)
  (with-compensations
    (let ((passwd*	(malloc-block/c sizeof-passwd))
	  (output*	(malloc-small/c))
	  (uid-int	(uid->integer uid))
	  (len		256))
      (let loop ((buf.len len)
		 (buf.ptr (malloc-block/c len)))
	(receive (result errno)
	    (platform:getpwuid_r uid-int passwd* buf.ptr buf.len output*)
	  (cond ((= errno ERANGE)
		 (let ((len (* 2 buf.len)))
		   (loop len (malloc-block/c len))))
		((= errno EINTR)
		 (loop buf.len buf.ptr))
		((pointer-null? (pointer-ref-c-pointer output* 0))
		 (raise-errno-error 'getpwuid errno uid))
		(else
		 (pointer->struct-passwd passwd*))))))))

(define (getpwnam user-name)
  (with-compensations
    (let ((passwd*	(malloc-block/c sizeof-passwd))
	  (output*	(malloc-small/c))
	  (name*	(string->cstring/c user-name))
	  (len		256))
      (let loop ((buf.len len)
		 (buf.ptr (malloc-block/c len)))
	(receive (result errno)
	    (platform:getpwnam_r name* passwd* buf.ptr buf.len output*)
	  (cond ((= errno ERANGE)
		 (let ((len (* 2 buf.len)))
		   (loop len (malloc-block/c len))))
		((= errno EINTR)
		 (loop buf.len buf.ptr))
		((pointer-null? (pointer-ref-c-pointer output* 0))
		 (raise-errno-error 'getpwnam errno user-name))
		(else
		 (pointer->struct-passwd passwd*))))))))

(define (fgetpwent stream)
  (with-compensations
    (let ((passwd*	(malloc-block/c sizeof-passwd))
	  (output*	(malloc-small/c))
	  (stream*	(FILE*->pointer stream))
	  (len		256))
      (let loop ((buf.len len)
		 (buf.ptr (malloc-block/c len)))
	(receive (result errno)
	    (platform:fgetpwent_r stream* passwd* buf.ptr buf.len output*)
	  (cond ((= errno ERANGE)
		 (let ((len (* 2 buf.len)))
		   (loop len (malloc-block/c len))))
		((= errno EINTR)
		 (loop buf.len buf.ptr))
		((= result ENOENT)
		 #f)
		((= 0 result)
		 (pointer->struct-passwd (pointer-ref-c-pointer output* 0)))
		(else
		 (raise-errno-error 'fgetpwent errno stream))))))))


;;;; groups database

(define (getgrgid gid)
  (with-compensations
    (let ((group*	(malloc-block/c sizeof-group))
	  (output*	(malloc-small/c))
	  (gid-int	(gid->integer gid))
	  (len		256))
      (let loop ((buf.len len)
		 (buf.ptr (malloc-block/c len)))
	(receive (result errno)
	    (platform:getgrgid_r gid-int group* buf.ptr buf.len output*)
	  (cond ((= errno ERANGE)
		 (let ((len (* 2 buf.len)))
		   (loop len (malloc-block/c len))))
		((= errno EINTR)
		 (loop buf.len buf.ptr))
		((pointer-null? (pointer-ref-c-pointer output* 0))
		 (raise-errno-error 'getgrgid errno gid))
		(else
		 (pointer->struct-group group*))))))))

(define (getgrnam group-name)
  (with-compensations
    (let ((group*	(malloc-block/c sizeof-group))
	  (output*	(malloc-small/c))
	  (name*	(string->cstring/c group-name))
	  (len		256))
      (let loop ((buf.len len)
		 (buf.ptr (malloc-block/c len)))
	(receive (result errno)
	    (platform:getgrnam_r name* group* buf.ptr buf.len output*)
	  (cond ((= errno ERANGE)
		 (let ((len (* 2 buf.len)))
		   (loop len (malloc-block/c len))))
		((= errno EINTR)
		 (loop buf.len buf.ptr))
		((pointer-null? (pointer-ref-c-pointer output* 0))
		 (raise-errno-error 'getgrnam errno group-name))
		(else
		 (pointer->struct-group group*))))))))

(define (fgetgrent stream)
  (with-compensations
    (let ((group*	(malloc-block/c sizeof-group))
	  (output*	(malloc-small/c))
	  (stream*	(FILE*->pointer stream))
	  (len		256))
      (let loop ((buf.len len)
		 (buf.ptr (malloc-block/c len)))
	(receive (result errno)
	    (platform:fgetgrent_r stream* group* buf.ptr buf.len output*)
	  (cond ((= errno ERANGE)
		 (let ((len (* 2 buf.len)))
		   (loop len (malloc-block/c len))))
		((= errno EINTR)
		 (loop buf.len buf.ptr))
		((= result ENOENT)
		 #f)
		((= 0 result)
		 (pointer->struct-group (pointer-ref-c-pointer output* 0)))
		(else
		 (raise-errno-error 'fgetgrent errno stream))))))))


;;;; done

)

;;; end of file
