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


(library (posix system primitives)
  (export
    sysconf		pathconf
    fpathconf		confstr
    gethostname		sethostname
    getdomainname	setdomainname
    uname
    mount		umount2			umount

    getuid		getgid
    geteuid		getegid
    setuid		seteuid
    setgid		setegid
    getgroups		setgroups
    initgroups		getgrouplist
    getlogin		cuserid
    getpwuid		getpwnam
    getgrgid		getgrnam
    fgetpwent		fgetgrent

    setenv getenv environ)
  (import (rnrs)
    (receive)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign errno)
    (foreign cstrings)
    (posix sizeof)
    (posix typedefs)
    (prefix (posix system platform) platform:))


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



;;;; mounting file systems

(define (mount special-file mount-point fstype options data*)
  (with-compensations
    (let ((data* (malloc-block/c)))
      (receive (result errno)
	  (platform:mount (string->cstring/c special-file)
			  (string->cstring/c mount-point)
			  (string->cstring/c fstype)
			  options
			  data*)
	(if (= -1 result)
	    (raise-errno-error 'mount errno (list special-file mount-point fstype options data*))
	  result)))))

(define (umount2 pathname flags)
  (with-compensations
    (receive (result errno)
	(platform:umount2 (string->cstring/c pathname) flags)
      (if (= -1 result)
	  (raise-errno-error 'umount2 errno (list pathname flags))
	result))))

(define (umount pathname)
  (with-compensations
    (receive (result errno)
	(platform:umount (string->cstring/c pathname))
      (if (= -1 result)
	  (raise-errno-error 'umount errno pathname)
	result))))


;;;; users accessors

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


;;;; users mutators

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


;;;; environment variables

(define setenv
  (case-lambda
   ((varname newvalue)
    (setenv varname newvalue #t))
   ((varname newvalue replace)
    (with-compensations
      (platform:setenv (string->cstring varname)
		       (string->cstring newvalue)
		       (if replace 1 0))))))

(define (getenv varname)
  (with-compensations
    (let ((p (platform:getenv (string->cstring varname))))
      (if (pointer-null? p)
	  #f
	(cstring->string p)))))

(define (environ)
  (argv->strings (platform:environ)))


;;;; done

)

;;; end of file
